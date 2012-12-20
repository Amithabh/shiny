library(shiny)

MyReactiveEnv <- setRefClass(
  'MyReactiveEnv',
  contains = 'ReactiveEnvironment',
  field = list(id='integer'),
  methods = list(
    nextId = function() { id <<- if(length(id)) id + 1L else 1L },
    reactivePuppet = function(fun){
      .rs$NewReactiveFunction(
        setupFunc=function(input,name){
          puppetName <- paste(name,as.character(nextId()),sep='')
          input[[puppetName]] <- as.character(nextId())
          e <- new.env()
          with(e,{
              puppetName <- puppetName
          })
          e
        },
        func=function(){
          cat('calling',puppetName,',input is',input[[puppetName]],'\n')
          fun()
        }
      )$getValue
    }
  )
)

rs <- ReactiveSystem$new()

rs$setupWith(
  function(input,output){
    output$puppet <- reactivePuppet(function() {
      cat('walks\n')
    })
  },
  MyReactiveEnv
)
rs$output$puppet$run()
rs$input$puppet1 <- 'foo'
rs$flush()
