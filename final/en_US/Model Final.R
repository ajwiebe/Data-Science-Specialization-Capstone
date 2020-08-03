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