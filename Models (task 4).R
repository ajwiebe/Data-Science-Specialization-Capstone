rm(twit_lines, twit, swears, replace_url, replace_reg, replace_aaa, news_lines, news, blog_lines, blog, twit_train2, 
   twit_df, tidy_swears, swear_words, news_train2, news_df, blogs_df, blog_train2)

##Make everything small, 90% coverages are very large
##Unigrams 
tidy_repo 

unigram_cover <- tidy_repo %>%
  count(word) %>% 
  filter(n > 10) %>% 
  arrange(desc(n))

saveRDS(unigram_cover, "/Users/ajwiebe/Capstone Project/final/en_US/unigram_cover.rds")

##Bigrams 
bigrams <- clean_samp %>% unnest_tokens(bigram, text, n = 2, token = "ngrams")

bigram_cover <- bigrams %>% 
  count(bigram) %>% 
  filter(n > 10) %>%
  arrange(desc(n))

bigram_words <- bigram_cover %>% separate(bigram, c("firstword", "secondword"), sep = " ")
saveRDS(bigram_cover, "/Users/ajwiebe/Capstone Project/final/en_US/begram_cover.rds")
saveRDS(bigram_words, "/Users/ajwiebe/Capstone Project/final/en_US/bigram_words.rds")

rm(bigrams, bigram_cover, bigram_words)
##Trigrams 
trigrams <- clean_samp %>% unnest_tokens(trigram, text, n = 3, token = "ngrams")

trigram_cover <- trigrams %>% 
  count(trigram) %>% 
  filter(n > 10) %>%
  arrange(desc(n))

trigram_words <- trigram_cover %>% separate(trigram, c("firstword", "secondword", "thirdword"), sep = " ")
saveRDS(trigram_cover, "/Users/ajwiebe/Capstone Project/final/en_US/trigram_cover.rds")
saveRDS(trigram_words, "/Users/ajwiebe/Capstone Project/final/en_US/trigram_words.rds")

##Quadgrams 
quadgrams <- clean_samp %>% unnest_tokens(quadgram, text, n = 4, token = "ngrams")

quadgram_cover <- quadgrams %>% 
  count(quadgram) %>% 
  filter(n > 10) %>%
  arrange(desc(n))

quadgram_words <- quadgram_cover %>% separate(quadgram, c("firstword", "secondword", "thirdword", "fourthword"), sep = " ")
saveRDS(quadgram_cover, "/Users/ajwiebe/Capstone Project/final/en_US/quadgram_cover.rds")
saveRDS(quadgram_words, "/Users/ajwiebe/Capstone Project/final/en_US/quadgram_words.rds")

##Clean old stuff
rm(clean_samp, bigrams, trigrams, quadgrams, tidy_repo)

##Write functions 
##Function should take input of one, two, or three words. If three words, match to quadgram and supply last word.
##If two words, match to trigram and supply last word, if one word, match to bigram and supply last word. If no match, supply most common unigram



