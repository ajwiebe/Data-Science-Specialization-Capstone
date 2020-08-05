##Creative Exploration 
##90% coverage files are very large and slow, instead using n > 10 for each of bigrams trigrams and quadgrams to get rid of many phrases that aren't frequently used
##Fivegrams were considered but likely too large for mobile processing
##Data pulled equally from twitter, blogs, and news in ordere to be accurate in multiple situations and eliminate bias

##Example models 

##First try cant take arguments longer than 3
##Returns unwanted NAs
test_func <- function(input) {
  input <- tibble(text = input)
  replace <-  "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace, ""))
  wrd_nm <- str_count(input, boundary("word"))
  words <- unlist(str_split(input, boundary("word")))
  words <- tolower(words)
  if(length(words)==3) {
    filter(quadgram_words, 
           firstword==words[1],
           secondword==words[2], 
           thirdword==words[3]) %>% head() -> out
    return(as.character(out[1,4]))
  } else if(length(words)==2) {
    filter(trigram_words, 
           firstword==words[1],
           secondword==words[2]) %>% head() -> out
    return(as.character(out[1,3]))
  } else if(length(words)==1) {
    filter(bigram_words, 
           firstword==words[1]) %>% head() -> out 
    return(as.character(out[1,2]))
  } else if(##no matches) {
    return(unigram_cover[1,1])
} 
}

##Second try can take arguments of any length, less accurate at predicting past fourth word
##Still returns unwanted NAs
##Its pretty fast and I'd be willing to settle for this 

test_func2 <- function(input) {
  input <- tibble(text = input)
  replace <-  "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace, ""))
  wrd_nm <- str_count(input, boundary("word"))
  words <- unlist(str_split(input, boundary("word")))
  words <- tolower(words)
  if(length(words) > 3) {
    polys <- filter(bigram_words, 
                    firstword==words[length(words)])
    polys %>% head() -> out
    return(as.character(out[1,2]))
  }
  if(length(words)==3) {
    quads <- filter(quadgram_words, 
                    firstword==words[wrd_nm-2],
                    secondword==words[wrd_nm-1], 
                    thirdword==words[wrd_nm]) 
    quads %>% head() -> out
    return(as.character(out[1,4]))
    ifelse(is.na(quads), return(as.character(unigram_cover[1,1])))
  } else if(length(words)==2) {
    tris <- filter(trigram_words, 
                   firstword==words[wrd_nm-1],
                   secondword==words[wrd_nm])
    tris %>% head() -> out
    return(as.character(out[1,3]))
    ifelse(is.na(tris), return(as.character(unigram_cover[1,1])))
  } else if(length(words)==1) {
    bis <- filter(bigram_words, 
                  firstword==words[wrd_nm])
    bis %>% head() -> out
    return(as.character(out[1,2]))
    ifelse(is.na(bis), return(as.character(unigram_cover[1,1])))
  } else return(unigram_cover[1,1])
}

##Thrid try, simplified for ease of working
##Takes argument of any length, slightly more accurate past four words
##Returns an error if words are not part of model
##Error suggests most common unigram

test_func3 <- function(input) {
  input <- tibble(text = input)
  replace <-  "[^[:alpha:][:space:]]*"
  input <- input %>% mutate(text = str_replace_all(text, replace, ""))
  wrd_nm <- str_count(input, boundary("word"))
  words <- unlist(str_split(input, boundary("word")))
  words <- tolower(words)
  if(length(words) > 3) {
    polys <- filter(trigram_words,
                    firstword==words[length(words)])
    polys %>% head() -> out
    if(is.na(out[1,2]==TRUE)) stop("Word(s) not recognized", ", suggested word:", unigram_cover[1,1])
    return(as.character(out[1,2]))
  } 
}


##Fourth try
##This one returns two words instead of one 
##Not helpful

test_func4 <- function(input) {
  input <- tibble(text = input)
  replace <-  "[^[:alpha:][:space:]]*"
  input <- input %>% mutate(text = str_replace_all(text, replace, ""))
  wrd_nm <- str_count(input, boundary("word"))
  words <- unlist(str_split(input, boundary("word")))
  words <- tolower(words)
  if(length(words) > 3) {
    polys <- filter(quadgram_words,
                    firstword==words[length(words)],
                    secondword==words[length(words)-1])
    polys %>% head() -> out
    return(as.character(out[1,3:4]))
  } else if(is.na()) {
    return(as.character(unigram_cover[1,1]))
  }
}

##Final Model
word_predictor <- function(input) {
  input <- tibble(text = input)
  replace <-  "[^[:alpha:][:space:]]*"
  input <- input %>% mutate(text = str_replace_all(text, replace, ""))
  wrd_nm <- str_count(input, boundary("word"))
  words <- unlist(str_split(input, boundary("word")))
  words <- tolower(words)
  if(length(words) > 3) {
    polys <- filter(trigram_words,
                    firstword==words[length(words)])
    polys %>% head() -> out
    if(is.na(out[1,2]==TRUE)) stop("Word(s) not recognized", ", suggested word:", unigram_cover[1,1])
    return(as.character(out[1,2]))
  } else if(length(words)==3) {
    quads <- filter(quadgram_words, 
                    firstword==words[wrd_nm-2],
                    secondword==words[wrd_nm-1], 
                    thirdword==words[wrd_nm]) 
    quads %>% head() -> out
    if(is.na(out[1,4])==TRUE) stop("Word(s) not recognized", ", suggested word:", unigram_cover[1,1])
    return(as.character(out[1,4]))
  } else if(length(words)==2) {
    tris <- filter(trigram_words, 
                   firstword==words[wrd_nm-1],
                   secondword==words[wrd_nm])
    tris %>% head() -> out
    if(is.na(out[1,3])==TRUE) stop("Word(s) not recognized", ", suggested word:", unigram_cover[1,1])
    return(as.character(out[1,3]))
  } else if(length(words)==1) {
    bis <- filter(bigram_words, 
                  firstword==words[wrd_nm])
    bis %>% head() -> out
    if(is.na(out[1,2])==TRUE) stop("Word(s) not recognized", ", suggested word:", unigram_cover[1,1])
    return(as.character(out[1,2]))
  }  
}

##Some Graphs 
##Unigram
png("unigrams.png", width = 700, height = 500)
cover_90 <- readRDS("cover_90.rds")
cover_90 %>% top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) + 
  geom_col() + xlab(NULL) + geom_bar(fill = "red", color = "black", stat = "identity")
dev.off()

##Bigram
png("bigrams.png", width = 900, height = 500)
bigram_cover_90 <- readRDS("bigram_cover_90.rds")
bigram_cover_90 %>% top_n(20, proportion) %>% 
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) + 
  geom_col() + xlab(NULL) + geom_bar(fill = "orange", color = "black", stat = "identity" )
dev.off()

##Trigram
png("trigrams.png", width = 1100, height = 500)
trigram_cover_90 <- readRDS("trigram_cover_90.rds")
trigram_cover_90 %>% top_n(20,proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) + 
  geom_col() + xlab(NULL) + geom_bar(fill = "yellow", color = "black", stat = "identity")
dev.off()

##Quadgram
png("quadgrams.png", width = 1300, height = 500)
quadgram_cover_90 <- readRDS("quadgram_cover_90.rds")
quadgram_cover_90 %>% top_n(20, proportion) %>%
    mutate(quadgram = reorder(quadgram, proportion)) %>%
    ggplot(aes(quadgram, proportion)) + 
  geom_col() + xlab(NULL) + geom_bar(fill = "green", color = "black", stat = "identity")
dev.off()

