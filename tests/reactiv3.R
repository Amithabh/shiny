library(shiny)

MyReactiveEnv <- setRefClass(
  'MyReactiveEnv',
  contains = 'ReactiveEnvironment',
  field = list(id='integer'),
  methods = list(
    .nextId = function() { id <<- if(length(id)) id + 1L else 1L },
    reactivePuppet = function(fun){
      .rs$NewReactiveFunction(
        setupFunc=function(envir=NULL,input=NULL,name=NULL){
          puppetName <- paste(name,as.character(.nextId()),sep='')
          input[[puppetName]] <- as.character(.nextId())
          e <- new.env()
          with(e,{
              input <- input
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

rs$define(
  function(input,output){
    output$puppet <- reactivePuppet(function() {
      cat('walks\n')
    })
  },
  MyReactiveEnv
)
rs$output$puppet()
rs$input$puppet1 <- 'foo'
rs$flush()
