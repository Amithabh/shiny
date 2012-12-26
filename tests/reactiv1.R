library(shiny)

rs <- ReactiveSystem$new()
rs$define(
  function(input,output){
    re <- reactive(function(){
      cat("re():input$n(",input$n,")\n")
      input$n
    })

    output$ntext <- reactive(function() {
      a <- re()
      b <- a*2
      cat("output$ntext(): re()(",a,")*2=",b,'\n')
      'foo'
    })
  }
)
rs$input$n <- 1 
o <- rs$NewReactiveObserver(rs$output$ntext)
o$observeWith(function() cat("ntext calling\n"))
o$invalidate()
rs$flush()
for (i in 2:10){
  rs$input$n <- i
  rs$flush()
}
