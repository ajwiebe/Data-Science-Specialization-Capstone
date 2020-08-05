##Load the files
setwd("/Users/ajwiebe/Capstone Project/final/en_US")
twit <- file("en_US.twitter.txt")
twit_lines <- readLines(twit)
blog <- file("en_US.blogs.txt")
blog_lines <- readLines(blog)
news <- file("en_US.news.txt")
news_lines <- readLines(news)

twit_words <- wordcount(twit_lines, sep = " ")
blog_words <- wordcount(blog_lines, sep = " ")
news_words <- wordcount(news_lines, sep = " ")
data.frame(type = c("twitter", "blog", "news"), words = c(length(twit_words), length(blog_words), length(news_words)))

twit_df <- data.frame(text = twit_lines)
blogs_df <- data.frame(text = blog_lines)
news_df <- data.frame(text = news_lines)

##Subsets from twitter
set.seed(25)
twit_train2 <- twit_df %>% sample_n(., nrow(twit_df)*0.1)
##Subset from blogs 
blog_train2 <- blogs_df %>% sample_n(., nrow(blogs_df)*0.1)
##Subset from news
news_train2 <- news_df %>% sample_n(., nrow(news_df)*0.1)

##Make the repository 
repo_samp <- bind_rows(mutate(twit_train2, source = "twit_df"), mutate(blog_train2, source = "blogs_df"), mutate(news_train2, source = "news_df"))
repo_samp$source <- as.factor(repo_samp$source)

##Getting rid of the bad stuff
swears <- c("ass", "bitch", "cunt", "dick", "fuck", "nigger", "nigga", "retard", "shit", "damn", "cock", "coon", "fag")
swear_words <- tibble(line = 1:13, text = swears)
tidy_swears <- swear_words %>% unnest_tokens(word, text)

replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"

##Clean the repo
clean_samp <- repo_samp %>% mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

tidy_repo <- clean_samp %>%
  unnest_tokens(word, text) %>% anti_join(stop_words)

saveRDS(clean_samp, "/Users/ajwiebe/Capstone Project/final/en_US/clean_samp.rds")

##Predict coverage 
cover_50 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.5)
nrow(cover_50)

cover_90 <- tidy_repo %>%
  count(word) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(cover_90)

##Saving
saveRDS(tidy_repo, "/Users/ajwiebe/Capstone Project/final/en_US/tidy_repo.rds")
saveRDS(cover_90, "/Users/ajwiebe/Capstone Project/final/en_US/cover_90.rds")

##Graphs 
cover_90 %>%
  top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) +
  geom_col() +
  xlab(NULL) + geom_bar(fill = "pink", color = "black", stat = "identity")

##Looking at bigrams and trigrams 
clean_samp <- as_tibble(clean_samp)
clean_samp <- na.omit(clean_samp)
bigrams <- clean_samp  %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_cover_90 <- bigrams %>%
  count(bigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(bigram_cover_90)

bigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) +
  geom_col() +
  xlab(NULL) + geom_bar(fill = "blue", color = "black", stat = "identity")

saveRDS(bigram_cover_90, "/Users/ajwiebe/Capstone Project/final/en_US/bigram_cover_90.rds")

##TRIGRAMS
trigrams <- clean_samp %>% unnest_tokens(trigram, text, n = 3, token = "ngrams")

trigram_cover_90 <- trigrams %>%
  count(trigram) %>%  
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion)) %>%  
  mutate(coverage = cumsum(proportion)) %>%
  filter(coverage <= 0.9)
nrow(trigram_cover_90)

##Trigrams graph 
trigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) +
  geom_col() +
  xlab(NULL) + geom_bar(fill = "green", color = "black", stat = "identity")

saveRDS(trigram_cover_90, "/Users/ajwiebe/Capstone Project/final/en_US/trigram_cover_90.rds")

##Quadgrams 
quadgrams <- clean_samp %>% unnest_tokens(quadgram, text, n = 4, token = "ngrams")

quadgram_cover_90 <- quadgrams %>% count(quadgram) %>% 
  mutate(proportion = n / sum(n)) %>% 
  arrange(desc(proportion)) %>% 
  mutate(coverage = cumsum(proportion)) %>% 
  filter(coverage <= 0.9)

quadgram_cover_90 %>% top_n(20, proportion) %>% 
  mutate(quadgram = reorder(quadgram, proportion)) %>%
  ggplot(aes(quadgram, proportion)) + 
  geom_col() + 
  xlab(NULL) + geom_bar(fill = "purple", color = "black", stat = "identity")

saveRDS(quadgram_cover_90, "/User/ajwiebe/Capstone Project/final/en_US/quadgram_cover_90.rds")

rm(bigram_cover_90, trigram_cover_90, quadgram_cover_90, cover_90)






