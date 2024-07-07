#Load packages
library(tm)
library(stringi) # to help remove profanity efficiently
library(tokenizers)
library(dplyr)
library(stringr)
library(hunspell)
library(parallel)
library(Matrix)
library(slam)

# define the file paths
blog_file <- "data/en_US/en_US.blogs.txt"
news_file <- "data/en_US/en_US.news.txt"
twitter_file <- "data/en_US/en_US.twitter.txt"

#read the 3 files
blogs <-readLines(blog_file, skipNul = TRUE)
news <- readLines(news_file, skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#create full dataset
corpora <- c(blogs,news,twitter)

#To create training sample:

#define size of samples
n <- 100000
#set seed for reproducibility
set.seed(123)
#draw training corpora
training <- sample(corpora, n)
#remove heavy files no longer needed from workspace
rm(blogs, news, twitter, corpora)

#Pre-process the data

# 1) Load a list of common words and Convert to a named vector for faster lookup
common_words <- readLines("data/wordlist.10000.txt")
common_words <- setNames(rep(TRUE, length(common_words)), common_words)

# Define the function to correct typos using hunspell, but only for uncommon words
correct_typos <- function(word) {
        if (!is.null(common_words[word])) {
                return(word)
        }
        suggestions <- hunspell_suggest(word)[[1]]
        if (length(suggestions) > 0) {
                return(suggestions[1])  # Choose the first suggestion
        } else {
                return(word)  # If no suggestions, return the original word
        }
}

# Define the function to remove noise
remove_noise <- function(text) {
        # Remove long sequences of the same character (e.g., "aaaaaa")
        text <- gsub("(.)\\1{4,}", "", text)
        
        # Remove random alphanumeric sequences (e.g., "aaaaaaadasdasda")
        text <- gsub("\\b[a-zA-Z0-9]{10,}\\b", "", text)
        
        return(text)
}

# Define the function to preprocess text
preprocess_text <- function(text) {
        text <- tolower(text)
        text <- gsub("[[:digit:]]", "", text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub("\\s+", " ", text)
        text <- trimws(text)
        words <- unlist(strsplit(text, "\\s+"))
        words <- sapply(words, correct_typos, USE.NAMES = FALSE)
        text <- paste(words, collapse = " ")
        text <- remove_noise(text)
        return(text)
}

# Set up parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
clusterExport(cl, c("preprocess_text", "correct_typos", "remove_noise", "common_words", "hunspell_suggest"))

# Preprocess the corpus using parallel processing
training <- parSapply(cl, training, preprocess_text)

# Stop the cluster
stopCluster(cl)

# Convert to Corpus
training_corpus <- Corpus(VectorSource(training))

#Pre-process the data
# remove "/", "@", "|", etc and replace them with space
#define function to perform the substitution
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ",x))
patterns_to_remove <- c("/", "@\\S+", "@", "\\|", "http\\S+", "[[:cntrl:]]+", "#\\S+", "’") #removes /, twitter handles, @, |, URLs, controls, hashtags, apostrophes 

for (pattern in patterns_to_remove) {
        training_corpus <- tm_map(training_corpus, toSpace, pattern)
}

# Text stemming and eliminating extra white spaces
training_corpus <- tm_map(training_corpus, stripWhitespace)
training_corpus <- tm_map(training_corpus, stemDocument)

#Split training corpus into 2 versions: with and without top words
training_corpus_no_stop <- tm_map(training_corpus, removeWords, stopwords("english"))

#read in profanity list
#profanity_words <- readLines("data/profanity_list.txt")
# Combine all profanity words into a single regular expression pattern
# Using word boundaries (\b) to ensure whole words are matched
#profanity_pattern <- paste0("\\b(", paste(profanity_words, collapse = "|"), ")\\b")
# Define a function to remove profanity words using stringi
#remove_profanity <- content_transformer(function(text, pattern) {
 #       stri_replace_all_regex(text, pattern, "", vectorize_all = FALSE, case_insensitive = TRUE)
#})
# Apply profanity removal function
#training_corpus <- tm_map(training_corpus, remove_profanity, profanity_pattern)

# Convert corpus to character vector
training_text_no_stop <- sapply(training_corpus_no_stop, as.character)
training_text <- sapply(training_corpus, as.character)

# Sample 2% of the corpus without stop words for co-occurrence analysis
set.seed(42)  # For reproducibility
sample_size <- floor(0.02 * length(training_text_no_stop))
sample_indices <- sample(seq_len(length(training_text_no_stop)), size = sample_size)
sampled_text <- training_text_no_stop[sample_indices]

# Create a document-term matrix
dtm_sample <- DocumentTermMatrix(Corpus(VectorSource(sampled_text)))

# Convert to a sparse matrix and calculate co-occurrence
term_matrix_sample <- as.matrix(dtm_sample)
co_occurrence_sample <- t(term_matrix_sample) %*% term_matrix_sample
diag(co_occurrence_sample) <- 0  # Remove self-co-occurrences

# Convert the co-occurrence matrix to a tidy data frame
co_occurrence_df <- as.data.frame(as.table(as.simple_triplet_matrix(co_occurrence_sample)))
colnames(co_occurrence_df) <- c("term1", "term2", "Freq")
co_occurrence_df <- co_occurrence_df %>%
        filter(Freq > 0) %>%
        arrange(desc(Freq))

# Function to create n-grams - wrapper around tokenize_ngrams that sets simplify=TRUE; created so we can later change the function if necessary
create_ngrams <- function(text, n) {
        tokenize_ngrams(text, n = n, simplify = TRUE)
}
# Create n-grams
#unigrams include stop words
unigrams <- create_ngrams(training_text, 1)
#bigrams including stop words
bigrams <- create_ngrams(training_text, 2)
#bigrams without stop words
bigrams_no_stop  <- create_ngrams(training_text_no_stop,2) 
# Convert n-grams to tibbles for easier manipulation
unigrams_tbl <- tibble(ngram = unlist(unigrams))
bigrams_tbl <- tibble(ngram = unlist(bigrams))
bigrams_no_stop_tbl <- tibble(ngram = unlist(bigrams_no_stop))

#remove n-gram corpus docs
rm(unigrams, bigrams, bigrams_no_stop)

# Prune n-grams with a low frequency threshold
frequency_threshold <- 3

# Calculate frequency of n-grams, create context columns, and calculate conditional probs by context
unigrams_tbl <- unigrams_tbl %>%
        count(ngram, name = "freq") %>%
        filter(freq >= frequency_threshold) %>%
        mutate(prob = (freq + 1) / (sum(freq) + n()))  # Add-1 smoothing

bigrams_tbl <- bigrams_tbl %>%
        count(ngram, name = "freq") %>%
        filter(freq >= frequency_threshold) %>%
        distinct(ngram, .keep_all = TRUE) %>%
        mutate(context = word(ngram, 1)) %>%
        group_by(context) %>%
        mutate(prob = (freq + 1) / (sum(freq) + n())) %>%  # Add-1 smoothing
        ungroup()

bigrams_no_stop_tbl <- bigrams_no_stop_tbl %>%
        count(ngram, name = "freq") %>%
        filter(freq >= frequency_threshold) %>%
        distinct(ngram, .keep_all = TRUE) %>%
        mutate(context = word(ngram, 1)) %>%
        group_by(context) %>%
        mutate(prob = (freq + 1) / (sum(freq) + n())) %>%  # Add-1 smoothing
        ungroup()

# Define a function using a backoff model to offer 6 suggestions: 2 from bigrams without stop words, 2 from the bigrams with stop words, 2 from the co-occurrence matrix. 
predict_next_word <- function(sentence, unigrams_tbl, bigrams_tbl, bigrams_no_stop_tbl, co_occurrence_df) {
        # Preprocess the sentence
        sentence <- tolower(sentence)
        sentence <- gsub("[[:digit:]]", "", sentence)
        sentence <- gsub("[[:punct:]]", "", sentence)
        sentence <- gsub("\\s+", " ", sentence)
        sentence <- trimws(sentence)
        sentence_corpus <- Corpus(VectorSource(sentence)) #convert to corpus for stemming
        sentence_corpus <- suppressWarnings(tm_map(sentence_corpus,stemDocument)) #stemming
        sentence <- sapply(sentence_corpus, as.character)
        
        # Split the sentence into words
        words <- unlist(strsplit(sentence, " "))
        num_words <- length(words)
        
        suggestions <- character(0)
        
        if (num_words >= 1) {
                last_word <- words[num_words]
                
                # 1. Try to find suggestions from bigrams_no_stop_tbl
                bigrams_no_stop_suggestions <- bigrams_no_stop_tbl %>%
                        filter(context == last_word) %>%
                        arrange(desc(prob)) %>%
                        head(2) %>%
                        pull(ngram) %>%
                        word(2)
                
                suggestions <- unique(c(suggestions, bigrams_no_stop_suggestions))
                
                # 2. Try to find suggestions from bigrams_tbl
                bigrams_suggestions <- bigrams_tbl %>%
                        filter(context == last_word) %>%
                        arrange(desc(prob)) %>%
                        head(2) %>%
                        pull(ngram) %>%
                        word(2)
                
                suggestions <- unique(c(suggestions, bigrams_suggestions))
        }
        
        # 3. Try to find suggestions from co_occurrence_df
        co_occurrence_suggestions <- co_occurrence_df %>%
                filter(term1 %in% words | term2 %in% words) %>%
                arrange(desc(Freq)) %>%
                head(2) %>%
                mutate(word = if_else(term1 %in% words, term2, term1)) %>%
                pull(word)
        
        suggestions <- unique(c(suggestions, co_occurrence_suggestions))
        
        # Remove invalid suggestions (e.g., digits, NA, empty strings)
        suggestions <- suggestions[!is.na(suggestions) & suggestions != "" & !grepl("[[:digit:]]", suggestions)]
        
        # 4. If fewer than 6 suggestions, fill with unigrams_tbl
        if (length(suggestions) < 6) {
                unigram_suggestions <- unigrams_tbl %>%
                        arrange(desc(prob)) %>%
                        head(6 - length(suggestions)) %>%
                        pull(ngram)
                
                suggestions <- unique(c(suggestions, unigram_suggestions))
        }
        
        # Return the top 6 unique suggestions
        return(suggestions[1:6])
}


# Example usage
sentence <- "He went to the"
suggestions <- predict_next_word(sentence, unigrams_tbl, bigrams_tbl, bigrams_no_stop_tbl, co_occurrence_df)
print(suggestions)

## Save data tables
#set new working directory for app
setwd("/Users/tacit/Library/CloudStorage/OneDrive-Personal/Training/R_training/Course10/capstone/prediction_app")
# Save each object as an RDS file with compression
saveRDS(unigrams_tbl, file = "data/unigrams_tbl.rds", compress = "xz")
saveRDS(bigrams_tbl, file = "data/bigrams_tbl.rds", compress = "xz")
saveRDS(bigrams_no_stop_tbl, file = "data/bigrams_no_stop_tbl.rds", compress = "xz")
saveRDS(co_occurrence_df, file = "data/co_occurrence_df.rds", compress = "xz")
