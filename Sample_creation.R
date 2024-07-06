# this file documents how the sample for exploratory analysis was created

# define the file paths
blog_file <- "data/en_US/en_US.blogs.txt"
news_file <- "data/en_US/en_US.news.txt"
twitter_file <- "data/en_US/en_US.twitter.txt"

# function to read a sample of lines from a file
n <- 10000
read_sample <- function(file_path, sample_size = n) {
        lines <-readLines(file_path)
        sample(lines, sample_size)
}

# read samples from each file
blog_sample <- read_sample(blog_file)
news_sample <- read_sample(news_file)
twitter_sample <- read_sample(twitter_file)

# Combine samples into a single dataset
sample <- c(blog_sample, news_sample, twitter_sample)

# write the combined sample to a text file
output_file <- "data/sample.txt"
writeLines(sample, con = output_file)

