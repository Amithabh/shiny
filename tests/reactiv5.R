library(shiny)

rs <- ReactiveSystem$new()
rs$with({
  input <- ReactiveValues()
  output <- NewMap()
  setup <- function(input,output){
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
  setup(input,output)
})
input <- rs$with({input})
output <- rs$with({output})
input$n <- 1 
o <- rs$NewReactiveObserver(output$ntext)
o$observeWith(function() cat("ntext calling\n"))
o$invalidate()
rs$flush()
for (i in 2:10){
  input$n <- i
  rs$flush()
}
