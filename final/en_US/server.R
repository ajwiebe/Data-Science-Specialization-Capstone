server = shinyServer(function(input, output) {
  output$word_predictor_output <- renderText({
    word_predictor(input$input)
  })
}
  )