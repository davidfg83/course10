# define the file paths
blog_file <- "data/en_US/en_US.blogs.txt"
news_file <- "data/en_US/en_US.news.txt"
twitter_file <- "data/en_US/en_US.twitter.txt"

#laod the data
twitter_data <- readLines(twitter_file)
news_data <- readLines(news_file)
blog_data <- readLines(blog_file)

#2. Count the number of lines in the Twitter file
length(twitter_data)

#3. Find the length of the longest line (twitter not an option in multiple choice)
line_lengths_news <- nchar(news_data)
max(line_lengths_news)

line_lengths_blog <- nchar(blog_data)
max(line_lengths_blog)

#4. Count the number of lines where the word "love" (lowercase) appears
num_love_lines <- sum(grepl("\\blove\\b", twitter_data, ignore.case = FALSE))
num_hate_lines <- sum(grepl("\\bhate\\b", twitter_data, ignore.case = FALSE))
num_love_lines/num_hate_lines

#5. Print the line where the word "biostats" appears
biostats_line <- twitter_data[grep("\\bbiostats\\b", twitter_data, ignore.case = TRUE)]
biostats_line

#6. Count how many tweets contain the exact sentence
exact_sentence <- "A computer once beat me at chess, but it was no match for me at kickboxing"
sum(twitter_data == exact_sentence)
