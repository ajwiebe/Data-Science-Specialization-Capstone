source("Model Final.R")
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
library(shiny)
unigram_cover <- readRDS("unigram_cover.rds")
bigram_words <- readRDS("bigram_words.rds")
trigram_words <- readRDS("trigram_words.rds")
quadgram_words <- readRDS("quadgram_words.rds")


server = shinyServer(function(input, output) {
  output$word_predictor_output <- renderText({
    word_predictor(input$input)
  })
}
  )


