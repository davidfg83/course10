#exploratory analysis

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)

#SINGLE WORDS
# load the data
word_sample <- tibble(word = readLines("data/word_sample.txt"))
#calculate word frequencies
word_frequencies <- word_sample %>% count(word, sort = TRUE)
word_frequencies <- word_frequencies %>% mutate(share = round(n / nrow(word_sample) * 100,2))
# plot the n most frequent words
top_n <- 50
top_words <- word_frequencies %>% head(top_n)
ggplot(top_words, aes(x=reorder(word,share), y = share)) + geom_bar(stat = "identity") + 
        coord_flip() + labs(title = paste(top_n, "Most Frequent Words in Sample"),
                            x = "Words", y = "Relative Frequency") + theme_minimal()
#create a word cloud
set.seed(1)
wordcloud(words = word_frequencies$word, freq = word_frequencies$n,
          max.words = 250, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#BIGRAMS
#load the data
bigrams <- tibble(bigram = readLines("data/bigram_sample.txt"))
#calculate bigram frequencies
bigram_frequencies <- bigrams %>% count(bigram, sort = TRUE)
bigram_frequencies <- bigram_frequencies %>% mutate(share = round(n/nrow(bigrams) * 100, 2))
# plot the n most frequent bigrams
top_n_bigrams <- 20
top_bigrams <- bigram_frequencies %>% head(top_n_bigrams)
#plot the top bigrams
ggplot(top_bigrams, aes(x = reorder(bigram, share), y = share)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste(top_n_bigrams, "Most Frequent Bigrams"),
             x = "Bigrams",
             y = "Relative Frequency") +
        theme_minimal()

#TRIGRAMS
#load the data
trigrams <- tibble(trigram = readLines("data/trigram_sample.txt"))
#calculate trigram frequencies
trigram_frequencies <- trigrams %>% count(trigram, sort = TRUE)
trigram_frequencies <- trigram_frequencies %>% mutate(share = round(n/nrow(trigrams) * 100, 3))
# plot the n most frequent trigrams
top_n_trigrams <- 50
top_trigrams <- trigram_frequencies %>% head(top_n_trigrams)
#plot the top trigrams
ggplot(top_trigrams, aes(x = reorder(trigram, share), y = share)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste(top_n_bigrams, "Most Frequent Trigrams"),
             x = "Trigrams",
             y = "Relative Frequency") +
        theme_minimal()

#Dictionary coverage in sample
total_words <- sum(word_frequencies$n)
#calculate cumulative frequencies
word_frequencies <- word_frequencies %>% mutate(cumulative_freq = cumsum(n), cumulative_pct = cumulative_freq/total_words * 100)
#function to find the number of unique words needed for a certain percentage of coverage
word_coverage <- function(coverage_pct) {
        word <- word_frequencies %>%
                filter(cumulative_pct >= coverage_pct) %>%
                head(n=1)
        which(word_frequencies$word == as.character(word[1,1]))
}
#graph coverage by number of words
library(scales)
plot <- ggplot(word_frequencies, aes(x = seq_along(word), y = cumulative_pct)) +
        geom_line(color = "#0072B2", linewidth = 1.2) +
        labs(title = "Cumulative Coverage of Word Instances",
             x = "Number of Unique Words Included",
             y = "Cumulative Frequency of Word Instances") +
        theme_minimal() +
        scale_x_continuous(labels = comma, limits = c(0,10000)) + 
        scale_y_continuous(breaks = seq(0, 100, by = 10)) + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
plot

#assess use of foreign languages
#note: tried to use textcat, but it was too slow, and work on a subsample showed lots of language classification errors

#new strategy - use english dictionary and check if words in sample belong. dictionary from: https://github.com/dwyl/english-words/blob/master/words.txt
dictionary_file <- "data/words.txt"
english_dictionary <- tibble(word = readLines(dictionary_file))
#create temporary lowercase columns for comparison
word_sample <- word_sample %>%
        mutate(word_lower = tolower(word))
english_dictionary <- english_dictionary %>%
        mutate(word = tolower(word))
# Create a new column that removes possessive forms in the sample
word_sample <- word_sample %>%
        mutate(word_stripped = gsub("'s$", "", word_lower)) %>% 
        select(-word_lower)
english_dictionary <- english_dictionary %>%
        mutate(word_stripped = gsub("'s$", "", word)) %>% select(-word)
#keep only unique entries of dictionary (after removing caps and possessives)
english_dictionary <- tibble(word_stripped = unique(english_dictionary$word_stripped))
# initiate column in dictionary stating that word is in dictionary (will be used when joined with sample)
english_dictionary <- english_dictionary %>% mutate(on_dictionary = TRUE)
#perform left join to identify words in sample as on or off dictionary
word_sample <- word_sample %>% left_join(english_dictionary, by = "word_stripped") %>% 
        mutate(on_dictionary = ifelse(is.na(on_dictionary),FALSE,TRUE)) %>%
        select(-word_stripped)
#summarize words in and out of dictionary
table(word_sample$on_dictionary)

# Create a new column that lemmatizes words
library(textstem)
word_sample <- word_sample %>% mutate(word_lemmatized = lemmatize_words(word))

#calculate lemmatized word frequencies
lemmatized_frequencies <- word_sample %>% count(word_lemmatized, sort = TRUE)
lemmatized_frequencies <- lemmatized_frequencies %>% mutate(share = round(n / nrow(word_sample) * 100,2))

#Dictionary coverage in sample
total_lemmatized <- sum(lemmatized_frequencies$n)
#calculate cumulative frequencies
lemmatized_frequencies <- lemmatized_frequencies %>% mutate(cumulative_freq = cumsum(n), cumulative_pct = cumulative_freq/total_lemmatized * 100)

#graph of coverage of lemmatized words
plot <- ggplot(word_frequencies, aes(x = seq_along(word), y = cumulative_pct)) +
        geom_line(aes(color = "Original Words"), size = 1.2) +  # Label for legend
        labs(title = "Cumulative Coverage of Word Instances",
             x = "Number of Unique Words Included",
             y = "Cumulative Frequency of Word Instances") +
        theme_minimal() +
        scale_x_continuous(labels = comma, limits = c(0, 10000)) +  # Apply comma formatting
        scale_y_continuous(breaks = seq(0, 100, by = 10)) +
        theme(
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        )

plot + 
        geom_line(data = lemmatized_frequencies, aes(x = seq_along(word_lemmatized), y = cumulative_pct, color = "Lemmatized Words"), 
                  size = 1.2) +
        scale_color_manual(values = c("Original Words" = "#0072B2", "Lemmatized Words" = "#E69F00")) +
        labs(color = "Dictionary")  # Customize legend title
