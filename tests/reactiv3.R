library(shiny)

MyReactiveEnv <- setRefClass(
  'MyReactiveEnv',
  contains = 'ReactiveEnvironment',
  field = c('input'),
  methods = list(
    reactivePuppet = function(fun){
      reactive(function(){
          cat(.reactiveName,'(input is',input[[.reactiveName]],') ')
          fun()
        }
      )
    }
  )
)

rs <- ReactiveSystem$new()
rs$initializeEnvironment(MyReactiveEnv)
rs$envir$input <- rs$input <- rs$NewReactiveValues()

rs$withFunction(
  function(input,output){
    output$puppet1 <- reactivePuppet(function() {
      cat('walks\n')
    })
    output$puppet2 <- reactivePuppet(function() {
      cat('talks\n')
    })
  },
  MyReactiveEnv
)
rs$input$puppet1 <- 'foo'
rs$input$puppet2 <- 'bar'
rs$output$puppet1()
rs$output$puppet2()
rs$flush()
rs$input$puppet1 <- 'bee'
rs$input$puppet2 <- 'buz'
rs$flush()
