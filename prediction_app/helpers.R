# helpers.R

library(dplyr)
library(stringr)
library(tm)

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