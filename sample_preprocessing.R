# profanity list saved from https://www.cs.cmu.edu/~biglou/resources/bad-words.txt

# useful link from another student: https://rpubs.com/dawit3000/A_Prelude_to_Text_Mining

# Load necessary libraries
library(dplyr)
library(tidytext)
library(stringr)

# Define the file paths
sample_file <- "data/sample.txt"
profanity_file <- "data/profanity_list.txt"

# Load the sample data
sample_data <- readLines(sample_file)
# Convert sample data to a tibble for easier manipulation
sample_tibble <- tibble(text = sample_data)
#fix apostrophe discrepancy
sample_tibble <- sample_tibble %>% mutate(text = gsub("’", "'", text))

# Load the profanity list
profanity_list <- readLines(profanity_file)

# Function to remove profane words from a line of text
remove_profanity <- function(text, profanity_list) {
        profane_pattern <- paste0("\\b(", paste(profanity_list, collapse = "|"), ")\\b")
        str_replace_all(text, regex(profane_pattern, ignore_case = TRUE), "")
}

# Apply the function to remove profane words
cleaned_data <- sample_tibble %>%
        mutate(text = remove_profanity(text, profanity_list))


# Tokenize the data
# by word
tokenized_data <- cleaned_data %>% unnest_tokens(word, text)
#into bigrams
bigrams <- cleaned_data %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
#into trigrams
trigrams <- cleaned_data %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)


#Save the tokenized data to files for later use
output_file <- "data/word_sample.txt"
write.table(tokenized_data, file = output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
output_file <- "data/bigram_sample.txt"
write.table(bigrams, file = output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
output_file <- "data/trigram_sample.txt"
write.table(trigrams, file = output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
