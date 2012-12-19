#library(shiny)

rs <- ReactiveSystem$new()
rs$setupWith(
  function(input,output){
    re <- reactive(function(){
      cat("re():input$n(",input$n,")\n")
      input$n
    })

    output$ntext <- reactive(function() {
      a <- input$n
      b <- re()
      d <- a + b
      cat("output$ntext(): input$n(",a,")+re()(",b,")=",d,'\n')
      invisible()
    })
  }
)
rs$input$n <- 3
#rs$output$ntext$observeWith(function() cat("ntext calling\n"))
rs$output$ntext$run()
#rs$input$n <- 4
#rs$flush()
