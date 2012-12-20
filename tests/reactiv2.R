library(shiny)

rs <- ReactiveSystem$new()
rs$setupWith(
  function(input,output){
    re <- reactive(function(){
      cat("re():input$n(",input$n,")\n")
      input$n
    })

    output$ntext <- reactive(function() {
      a <- re()
      b <- input$n
      s <- a + b
      cat("output$ntext(): input$n(",b,")+ re()(",a,")=",s,'\n')
      invisible()
    })
  }
)
rs$input$n <- 1
rs$output$ntext$observeWith(function() cat("ntext calling\n"))
rs$output$ntext$run()
for (i in 2:10){
  rs$input$n <- i
  rs$flush()
}
for (i in 1:5) rs$flush()
