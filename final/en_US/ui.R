source("Model Final.R")
ui <- shinyUI(fluidPage(
  titlePanel("Text Predictor"), p("This app takes words from the textbox below and predicts the word to follow"),
  sidebarLayout(
    sidebarPanel(
      h2("If a word is not recognized by the predictor, it will show an error that suggests the most commonly used word"),
      textInput("input", h3("Input Words"), value = "Words"),
      submitButton(text = "Predict word", icon = NULL, width = NULL), 
      h3("Predicted word:"), 
      h4(em(span(textOutput("word_predictor_output"), style = "color:red"))),
      a("Source Code", href = "https://github.com/ajwiebe/Data-Science-Specialization-Capstone/blob/master/final/en_US/Model%20Final.R")
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Most Common Unigrams", 
               br(),
               img(src = "unigrams.png", width = 700, height = 500)), 
      tabPanel("Most Common Bigrams",
               br(),
               img(src = "bigrams.png", width = 900, height = 500)), 
      tabPanel("Most Common Trigrams", 
               br(), 
               img(src = "trigrams.png", width = 1100, height = 500)), 
      tabPanel("Most Common Quadgrams",
               br(), 
               img(src = "quadgrams.png", width = 1300, height = 500))
      )
    )
  ))
)