#library(shiny)

reactive <- ReactiveSystem$new()
reactive$setupWith(
  function(input,output){
    re <- reactive(function(){
      cat("re()\n")
      input$n
    })

    output$ntext <- reactive(function() {
      cat("output$ntext():",input$n,"* 2 =",input$n + re(),'\n')
      invisible()
    })
  }
)
#reactive$output$ntext$observeWith(function()cat('recalculating ntext\n')) 
reactive$input$n <- 3
reactive$output$ntext()
reactive$input$n <- 4
#reactive$flush()
