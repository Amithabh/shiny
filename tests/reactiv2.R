library(shiny)

rs <- ReactiveSystem$new()
rs$withFunction(
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
o <- rs$NewReactiveObserver(rs$output$ntext)
o$observeWith(function() cat("ntext calling\n"))
o$invalidate()
for (i in 2:10){
  rs$input$n <- i
  rs$flush()
}
for (i in 1:5) rs$flush()
