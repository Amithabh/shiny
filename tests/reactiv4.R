library(shiny)

id <- 0
reactiveSum <- function(fun){
  reactive(function() sum(fun()) )
}
registerReactive(reactiveSum=reactiveSum)

rs <- ReactiveSystem$new()

rs$withFunction(
  function(input,output){
    output$sumX <- reactiveSum(function(){
      cat('reactiveSum\n')
      input$x
    })
  }
)
rs$input$x <- rnorm(10)
cat(rs$output$sumX(),'\n')
rs$input$x <- rnorm(10)
rs$flush()
