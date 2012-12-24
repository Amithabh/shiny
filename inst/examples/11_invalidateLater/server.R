library(shiny)

shinyServer(function(input, output) {
   
  output$rand <- reactiveText(function() {
    invalidateLater(750)
    rnorm(1)
  })
  
})
