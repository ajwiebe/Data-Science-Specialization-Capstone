library(dplyr)
library(ggplot2)
library(tm)
library(corpus)
library(data.table)
library(tokenizers)
library(tidytext)
library(stringr)
library(ngram)
library(tidyr)

##Load the files
twit <- file("en_US.twitter.txt")
twit_lines <- readLines(twit)
blog <- file("en_US.blogs.txt")
blog_lines <- readLines(blog)
news <- file("en_US.news.txt")
news_lines <- readLines(news)
##Summary of characters in lines
summary(length(twit_lines))
summary(length(blog_lines))
summary(length(news_lines))
##Words to leave out of prediction 
swears <- c("ass", "bitch", "cunt", "dick", "fuck", "nigger", "nigga", "retard", "shit", "damn", "cock", "coon", "fag")

##Subsets from twitter
set.seed(15)
twit_train <- sample(twit_lines, length(twit_lines)*0.1)
##Subsets from blogs
blog_train <- sample(blog_lines, length(blog_lines)*0.1)
##Subsets from the news
news_train <- sample(news_lines, length(news_lines)*0.1)
##Collective testing and training data
train <- c(twit_train, blog_train, news_train)
##Save it 
writeLines(train, file = "/Users/ajwiebe/Capstone Project/final/en_US/train.txt")
saveRDS(train, file = "/Users/ajwiebe/Capstone Project/final/en_US/train.rds")

##Clean the train data
##TO DO: Remove bad words, remove punctuation, remove spaces, remove numbers, keep only words (all lowercase)

##Make it a corpus 
train_clean <- as.data.frame(train)
train_clean <- Corpus(VectorSource(train_clean))

##Remove non words
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
train_clean <- tm_map(train_clean, content_transformer(removeURL))
train_clean <- tm_map(train_clean, content_transformer(removeNumPunct))

##All lowercase 
train_clean <- tm_map(train_clean, content_transformer(tolower))

##Remove swears
swears <- as.data.frame(swears)
train_clean <- tm_map(train_clean, removeWords, swears[,1])

##Remove numbers 
train_clean <- tm_map(train_clean, removeNumbers)

##Remove puctuation 
train_clean <- tm_map(train_clean, removePunctuation)

##Remove space
train_clean <- tm_map(train_clean, stripWhitespace)

##Save the clean training set
saveRDS(train_clean, file = "/Users/ajwiebe/Capstone Project/final/en_US/train_clean.rds")

##Make it a matrix 
clean_matrix <- DocumentTermMatrix(train_clean, control = list(removePunctiation = T, stopwords = F))
clean_matrix <- removeSparseTerms(clean_matrix, sparse = 0.99)
sums <- colSums(as.matrix(clean_matrix))
m_features <- data.table(name = attributes(sums)$names, count = sums)
clean_feautures <- subset(m_features, count >= 2)

##Graph 
ggplot(clean_features[count > 50000], aes(name, count)) + geom_bar(fill = "red", color = "black", stat = "identity")

