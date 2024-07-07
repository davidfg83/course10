# helpers.R

library(dplyr)
library(stringr)
library(textstem)

# Preprocess text inputed by user
preprocess_text <- function(text) {
        text <- tolower(text)
        text <- gsub("[[:digit:]]", "", text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub("\\s+", " ", text)
        text <- trimws(text)
        text <- lemmatize_words(text)
        return(text)
}

# Define a function using a backoff model
predict_next_word <- function(sentence, unigrams_tbl, bigrams_tbl, trigrams_tbl, co_occurrence_df) {
        # Preprocess the sentence
        sentence <- preprocess_text(sentence)
        
        # Split the sentence into words
        words <- unlist(strsplit(sentence, " "))
        num_words <- length(words)
        
        if (num_words >= 2) {
                cont <- paste(words[(num_words-1):num_words], collapse = " ")
                trigram_suggestion <- trigrams_tbl %>%
                        filter(context == cont) %>%
                        arrange(desc(prob)) %>%
                        head(1) %>%
                        pull(ngram) %>%
                        word(3)
                
                if (length(trigram_suggestion) > 0) {
                        return(trigram_suggestion)
                }
        }
        
        if (num_words >= 1) {
                last_word <- words[num_words]
                
                bigrams_suggestion <- bigrams_tbl %>%
                        filter(context == last_word) %>%
                        arrange(desc(prob)) %>%
                        head(1) %>%
                        pull(ngram) %>%
                        word(2)
                
                if (length(bigrams_suggestion) > 0) {
                        return(bigrams_suggestion)
                }
        }
        
        co_occurrence_suggestion <- co_occurrence_df %>%
                filter(term1 %in% words | term2 %in% words) %>%
                arrange(desc(Freq)) %>%
                head(1) %>%
                mutate(word = if_else(term1 %in% words, term2, term1)) %>%
                pull(word)
        
        if (length(co_occurrence_suggestion) > 0) {
                return(co_occurrence_suggestion)
        }
        
        unigram_suggestion <- unigrams_tbl %>%
                arrange(desc(prob)) %>%
                head(1) %>%
                pull(ngram)
        
        return(unigram_suggestion)
}