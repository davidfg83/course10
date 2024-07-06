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

#To create training and testing samples:

#define size of samples
n <- 200000
n_training <- .75*n

#set seed for reproducibility
set.seed(123)
#draw working corpora
working_corpora <- sample(corpora, n)
#remove heavy files no longer needed from workspace
rm(blogs, news, twitter, corpora)

#sample n=training_size lines for the training set
train_indices <- sample(length(working_corpora), n_training)
training <- working_corpora[train_indices]
testing <- working_corpora[-train_indices]

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

#training <- tm_map(training, removeWords, stopwords("english"))
# Text stemming and eliminating extra white spaces
training_corpus <- tm_map(training_corpus, stripWhitespace)
training_corpus <- tm_map(training_corpus, stemDocument)


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
training_text <- sapply(training_corpus, as.character)

# Sample 2% of the corpus for co-occurrence analysis
set.seed(42)  # For reproducibility
sample_size <- floor(0.02 * length(training_text))
sample_indices <- sample(seq_len(length(training_text)), size = sample_size)
sampled_text <- training_text[sample_indices]

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
unigrams <- create_ngrams(training_text, 1)
bigrams <- create_ngrams(training_text, 2)
trigrams <- create_ngrams(training_text, 3)
fourgrams <- create_ngrams(training_text, 4)

# Convert n-grams to tibbles for easier manipulation
unigrams_tbl <- tibble(ngram = unlist(unigrams))
bigrams_tbl <- tibble(ngram = unlist(bigrams))
trigrams_tbl <- tibble(ngram = unlist(trigrams))
fourgrams_tbl <- tibble(ngram = unlist(fourgrams))

#remove n-gram corpus docs
rm(unigrams, bigrams, trigrams, fourgrams)

# Prune n-grams with a low frequency threshold
frequency_threshold <- 2

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

trigrams_tbl <- trigrams_tbl %>%
        count(ngram, name = "freq") %>%
        filter(freq >= frequency_threshold) %>%
        distinct(ngram, .keep_all = TRUE) %>%
        mutate(context = paste(word(ngram, 1), word(ngram, 2))) %>%
        group_by(context) %>%
        mutate(prob = (freq + 1) / (sum(freq) + n())) %>%  # Add-1 smoothing
        ungroup()

fourgrams_tbl <- fourgrams_tbl %>%
        count(ngram, name = "freq") %>%
        filter(freq >= frequency_threshold) %>%
        distinct(ngram, .keep_all = TRUE) %>%
        mutate(context = paste(word(ngram, 1), word(ngram, 2), word(ngram, 3))) %>%
        group_by(context) %>%
        mutate(prob = (freq + 1) / (sum(freq) + n())) %>%  # Add-1 smoothing
        ungroup()

# Function to predict the next word using a backoff model with Add-1 (Laplace) Smoothing and co-occurrence
predict_next_word <- function(sentence, unigrams_tbl, bigrams_tbl, trigrams_tbl, fourgrams_tbl, co_occurrence_df) {
        # Preprocess the input sentence to match the training data
        sentence <- tolower(sentence)
        sentence <- gsub("[[:digit:]]", "", sentence)
        sentence <- gsub("[[:punct:]]", "", sentence)
        sentence <- gsub("\\s+", " ", sentence)
        sentence <- trimws(sentence)
        
        # Split the sentence into words
        words <- unlist(strsplit(sentence, " "))
        num_words <- length(words)
        
        # Initialize prediction variable
        prediction <- NA
        found <- FALSE
        
        # Initialize a data frame to store the next word candidates and their probabilities
        next_word_probs <- data.frame(word = character(), prob = numeric(), stringsAsFactors = FALSE)
        
        # Check for 4-gram, trigram, and bigram contexts
        if (num_words >= 3) {
                last_three_words <- paste(words[(num_words-2):num_words], collapse = " ")
                possible_fourgrams <- fourgrams_tbl %>%
                        filter(context == last_three_words) %>%
                        select(ngram, prob)
                
                if (nrow(possible_fourgrams) > 0) {
                        possible_fourgrams <- possible_fourgrams %>%
                                mutate(word = word(ngram, 4)) %>%
                                select(word, prob)
                        
                        next_word_probs <- rbind(next_word_probs, possible_fourgrams)
                        found <- TRUE
                }
        }
        
        if (!found && num_words >= 2) {
                last_two_words <- paste(words[(num_words-1):num_words], collapse = " ")
                possible_trigrams <- trigrams_tbl %>%
                        filter(context == last_two_words) %>%
                        select(ngram, prob)
                
                if (nrow(possible_trigrams) > 0) {
                        possible_trigrams <- possible_trigrams %>%
                                mutate(word = word(ngram, 3)) %>%
                                select(word, prob)
                        
                        next_word_probs <- rbind(next_word_probs, possible_trigrams)
                        found <- TRUE
                }
        }
        
        if (!found && num_words >= 1) {
                last_word <- words[num_words]
                possible_bigrams <- bigrams_tbl %>%
                        filter(context == last_word) %>%
                        select(ngram, prob)
                
                if (nrow(possible_bigrams) > 0) {
                        possible_bigrams <- possible_bigrams %>%
                                mutate(word = word(ngram, 2)) %>%
                                select(word, prob)
                        
                        next_word_probs <- rbind(next_word_probs, possible_bigrams)
                        found <- TRUE
                }
        }
        
        # If no n-gram match is found, use co-occurrence
        if (!found) {
                co_occurrence_candidates <- co_occurrence_df %>%
                        filter(term1 %in% words | term2 %in% words) %>%
                        group_by(word = if_else(term1 %in% words, term2, term1)) %>%
                        summarize(prob = sum(Freq)) %>%
                        arrange(desc(prob))
                
                if (nrow(co_occurrence_candidates) > 0) {
                        next_word_probs <- rbind(next_word_probs, co_occurrence_candidates)
                        found <- TRUE
                }
        }
        
        # If no matches are found in n-grams or co-occurrence, use the most frequent unigrams as fallback
        if (!found) {
                fallback_unigrams <- unigrams_tbl %>%
                        arrange(desc(prob)) %>%
                        select(word = ngram, prob) %>%
                        head(10)  # Top 10 most frequent unigrams as fallback
                
                next_word_probs <- rbind(next_word_probs, fallback_unigrams)
        }
        
        # Aggregate probabilities for the same word
        next_word_probs <- next_word_probs %>%
                group_by(word) %>%
                summarize(prob = sum(prob)) %>%
                arrange(desc(prob))
        
        top_words <- next_word_probs %>% top_n(3, wt = prob) %>% pull(word)
        
        # Remove any names that were inadvertently set
        names(top_words) <- NULL
        
        return(top_words)
}


# Example usage:
sentence <- "the stadium of the"
top_words <- predict_next_word(sentence, unigrams_tbl, bigrams_tbl, trigrams_tbl, fourgrams_tbl, co_occurrence_df)
print(top_words)
