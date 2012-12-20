ReactiveSystem <- setRefClass(
  'ReactiveSystem',
  fields = c('.currentContext','.nextId', '.pendingInvalidate',
    '.objects','.envirClass', 'envir','input','output'),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 100L
      .pendingInvalidate <<- Map$new()
      .objects <<- Map$new()
      .envirClass <<- NULL
      envir <<- NULL
      input <<- NULL
      output <<- NULL
    },
    currentContext = function() {
      .currentContext
    },
    nextId = function() {
      .nextId <<- .nextId - 1L
      return(as.character(.nextId))
    },
    addPendingInvalidate = function(ctx) {
      #cat('add',ctx$id,'\n')
      .pendingInvalidate$set(ctx$id,ctx)
    },
    removePendingInvalidate = function(ctx){
      #cat('rem',ctx$id,'\n')
      .pendingInvalidate$remove(ctx$id)
    },
    isPendingInvalidate = function(ctx){
      .pendingInvalidate$containsKey(ctx$id)
    },
    runWith = function(ctx, func) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      func()
    },
    flush = function() {
      walkObjects(function(o){ 
        if (class(o) == 'ReactiveFunction'){
          o$.ctx$visited(FALSE)
        }
      })
      ctxKeys <- .pendingInvalidate$keys()
      while (length(ctxKeys) > 0) {
        ctx <- .pendingInvalidate$get(ctxKeys[1])
        ctx$debug()
        ctx$executeCallbacks()
        .pendingInvalidate$remove(ctxKeys[1])
        ctxKeys <- .pendingInvalidate$keys()
      }
    },
    NewDependencies = function(){
      Dependencies$new(.rs=.self)
    },
    NewContext = function(){
      Context$new(.rs=.self)
    },
    NewReactiveValues = function(){
      S3ReactiveValues(ReactiveValues$new(.rs=.self))
    },
    NewReactiveFunction = function(func=function(){},setupFunc=function(envir=NULL,input=NULL,name=NULL){}){
      x <- ReactiveFunction$new(func=func,setupFunc=setupFunc,.rs=.self)
      .objects$set(nextId(),x)
      x
    },
    NewReactiveObserver = function(func){
      x <- ReactiveObserver$new(func,.rs=.self)
      .objects$set(nextId(),x)
      x
    },
    initializeEnvironment = function(class=ReactiveEnvironment){
      if (is.object(class))
        superClass <- class$className
      else if (is.character(class))
        superClass <- class

      className <- paste('ReactiveClass__',digest(.self,algo='sha1'),sep='')
      RClassName <- paste('.__C__',className,sep='')
      if (exists(RClassName,globalenv())){
        .envirClass <<-getRefClass(className)
      } else {
        .envirClass <<- setRefClass(
          className,
          contains=c(superClass),
          where=globalenv()
        )
      }
      envir <<- .envirClass$new(.rs=.self)
    },
    setupEnvironmentWith = function(setupFun,class=ReactiveEnvironment){

      if (is.null(envir))
        initializeEnvironment(class=class)


      if (is.null(input))
        input <<- .self$NewReactiveValues()

      .envirClass$methods(setup=setupFun)

      outputFuns <- S3Map(Map$new())
      envir$setup(input=input,output=outputFuns)
      output <<- S3Map(Map$new())
      for (key in names(outputFuns)){
        f <- outputFuns[[key]]
        if (class(f)=='refMethodDef' && exists('.self',environment(f),inherits=FALSE)){
          obj <- get('.self',environment(f),inherits=FALSE)
          if('setName' %in% getRefClass(class(obj))$methods())
            obj$setName(key)
          if('setup' %in% getRefClass(class(obj))$methods())
            obj$setup()
        }
        output[[key]] <<- f
      }
      invisible()
    },
    show = function()cat('A Reactive System\n'),
    walkObjects = function(fun){
      invisible(lapply(.objects$mget(),fun))
    }
  )
)

ReactiveObject <- setRefClass(
  'ReactiveObject',
  fields = list(.rs='ReactiveSystem')
)

ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  contains=c('ReactiveObject'),
  methods = list(
    reactive = function(x,...){
      .rs$NewReactiveFunction(func=x)$getValue
    }
  )
)

Dependencies <- setRefClass(
  'Dependencies',
  contains = c('ReactiveObject'),
  fields = list(
    .dependencies = 'Map'
  ),
  methods = list(
    initialize = function(...){
      callSuper(...)
    },
    register = function(ctx=NULL) {
      if (!is.null(ctx)){
        ctx <- ctx
      } else if (is.null(ctx) && !is.null(.rs$currentContext())){
        ctx <- .rs$currentContext()
      } else {
        # Allow access to ReactiveValues outside of the environment
        #stop("No currentContext!\n")
        return()
      }
      if (!.dependencies$containsKey(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
      }
    },
    invalidate = function() {
      lapply(
        .dependencies$values(),
        function(ctx) {
          ctx$invalidateHint()
          ctx$invalidate()
          NULL
        }
      )
      invisible()
    },
    invalidateHint = function() {
      lapply(
        .dependencies$values(),
        function(dep.ctx) {
          dep.ctx$invalidateHint()
          NULL
        })
    },
    run = function(){
      lapply(
        .dependencies$values(),
        function(ctx) {
          ctx$executeCallbacks()
          NULL
        }
      )
      invisible()
    },
    contexts = function(){
      .dependencies
    }
  )
)

Context <- setRefClass(
  'Context',
  contains = c('ReactiveObject'),
  fields = list(
    id = 'character',
    .invalidatedHint = 'logical',
    .callbacks = 'list',
    .hintCallbacks = 'list',
    .dependants = 'ANY',
    .dependencies = 'ANY',
    .visited = 'logical'
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
      id <<- .rs$nextId()
      #cat('new ctx',id,'\n')
      .invalidatedHint <<- FALSE
      .dependants <<- .rs$NewDependencies()
      .dependencies <<- .rs$NewDependencies()
      .visited <<- FALSE
    },
    addDependant = function(){
      .dependants$register()
      depid <- ''
      if (!is.null(.rs$currentContext())){
        .rs$currentContext()$addDependency(.self)
        depid <- .rs$currentContext()$id
      }
      #cat('addDependant:',depid,'->',id,'\n')
    },
    addDependency = function(ctx){
      .dependencies$register(ctx)
    },
    invalidateDependants = function(){
      lhs <- paste(.dependants$contexts()$keys(),collapse=',')
      #cat('invalidateDependants[',lhs,']->',id,'\n')
      .dependants$invalidate()
    },
    run = function(func) {
      "Run the provided function under this context."
      .rs$runWith(.self, func)
    },
    runDependencies = function(){
      rhs <- paste(.dependencies$contexts()$keys(),collapse=',')
      #cat('runDependencies',id,'->[',rhs,']\n')
      .dependencies$run()
    },
    invalidateHint = function() {
      "Let this context know it may or may not be invalidated very soon; that
      is, something in its dependency graph has been invalidated but there's no
      guarantee that the cascade of invalidations will reach all the way here.
      This is used to show progress in the UI."
      if (.invalidatedHint)
        return()
      .invalidatedHint <<- TRUE
      .execute(.hintCallbacks)
      .dependants$invalidateHint()
    },
    invalidate = function() {
      "Schedule this context for invalidation. It will not actually be
        invalidated until the next call to \\code{\\link{flushReact}}."
      .rs$addPendingInvalidate(.self)
      NULL
    },
    isInvalidated = function(){
      .rs$isPendingInvalidate(.self)
    },
    validate = function(){
      .rs$removePendingInvalidate(.self)
    },
    onInvalidate = function(func) {
      "Register a function to be called when this context is invalidated.
        If this context is already invalidated, the function is called
        immediately."
      .callbacks <<- c(.callbacks, func)
      NULL
    },
    onInvalidateHint = function(func) {
      .hintCallbacks <<- c(.hintCallbacks, func)
    },
    clearInvalidatedHints = function(){
      .hintCallbacks <<- list()
      .invalidatedHint <<- TRUE
    },
    .execute = function(callbacks){
      "For internal use only."
      lapply(callbacks, function(func) {
        withCallingHandlers({
          func()
        }, warning = function(e) {
          # TODO: Callbacks in app
        }, error = function(e) {
          # TODO: Callbacks in app
        })
      })
    },
    executeCallbacks = function() {
      .execute(.callbacks)
    },
    executeHints = function() {
      .execute(.hintCallbacks)
    },
    visited = function(val=TRUE){
      if (missing(val))
        return(.visited)
      .visited <<- val
      if (!.visited)
        .invalidatedHint <<- FALSE
    },
    debug = function(){
      lhs <- paste(.dependants$contexts()$keys(),collapse=',')
      rhs <- paste(.dependencies$contexts()$keys(),collapse=',')
      #cat('[',lhs,']->(',id,')->[',rhs,']\n')
    }
  )
)

ReactiveValues <- setRefClass(
  'ReactiveValues',
  contains = c('ReactiveObject'),
  fields = list(
    .values = 'environment'
  ),
  methods = list(
    get = function(key) {
      if (!exists(key, where=.values, inherits=FALSE))
        return(NULL)

      .values[[key]]$dependants$register()
      .values[[key]]$val
    },
    set = function(key, value) {
      if (exists(key, where=.values, inherits=FALSE)) {
        if (identical(.values[[key]]$val, value)) 
          return(invisible(.values$key$val))
        .values[[key]]$val <<- value
        .values[[key]]$dependants$invalidate()
        .values[[key]]$dependants <<- .rs$NewDependencies()
      } else {
        assign(key,
          list(val=value,dependants=.rs$NewDependencies()), 
          pos=.values, inherits=FALSE)
      }

      invisible(value)
    },
    names = function() {
      ls(.values, all.names=TRUE)
    },
    mset = function(lst) {
      .values <<- new.env(parent=emptyenv())
      lapply(base::names(lst),
             function(name) {
               .self$set(name, lst[[name]])
             })
      invisible(lst)
    },
    mget = function() {
      lstNames <- names()
      lst <- lapply(lstNames,
          function(name) {
            .self$get(name)
      })
      names(lst) <- lstNames
      lst
    }
  )
)

S3ReactiveValues <- function(re){
  val = list(impl=re)
  class(val) <- 'reactiveValues'
  val
}

`$.reactiveValues` <- function(x,name){
  class(x) <- 'list'
  x[['impl']]$get(name)
}

`$<-.reactiveValues` <- function(x,name,value){
  class(x) <- 'list'
  x[['impl']]$set(name,value)
  class(x) <- 'reactiveValues'
  x
}

`[[.reactiveValues` <- function(x,name){
  class(x) <- 'list'
  x[['impl']]$get(as.character(name))
}

`[[<-.reactiveValues` <- function(x,name,value){
  class(x) <- 'list'
  x[['impl']]$set(as.character(name),value)
  class(x) <- 'reactiveValues'
  x
}

`<-.reactiveValues` <- function(x,value){
  if(!is('list',value)) stop("Value not a list!")
  class(x) <- 'list'
  invisible(x[['impl']]$mset(value))
  class(x) <- 'reactiveValues'
  x
}

as.list.reactiveValues <- function(x, ...) {
  class(x) <- 'list'
  x[['impl']]$mget()
}

names.reactiveValues <- function(x) {
  class(x) <- 'list'
  x[['impl']]$names()
}

print.reactiveValues <- function(x,...) print(unclass(as.list(x)),...)

.createValuesReader <- function(values) {
  acc <- list(impl=values)
  class(acc) <- 'reactvaluesreader'
  return(acc)
}

#' @S3method $ reactvaluesreader
`$.reactvaluesreader` <- function(x, name) {
  x[['impl']]$get(name)
}

#' @S3method names reactvaluesreader
names.reactvaluesreader <- function(x) {
  x[['impl']]$names()
}

#' @S3method as.list reactvaluesreader
as.list.reactvaluesreader <- function(x, ...) {
  x[['impl']]$mget()
}
print.reactvaluesreader <- function(x,...) print(unclass(as.list(x)),...)

ReactiveFunction <- setRefClass(
  'ReactiveFunction',
  contains = c('ReactiveObject'),
  fields = list(
    .name = 'character',
    .func = 'function',
    .setupFunc = 'function',
    .value = 'ANY',
    .ctx = 'ANY',
    .firstInvocation = 'logical',
    .isObserver ='logical',
    .observerFun = 'function'
  ),
  methods = list(
    initialize = function(func=function(){},setupFunc=function(envir=NULL,input=NULL,name=NULL){},...) {
      callSuper(...)
      .func <<- func
      .setupFunc <<- setupFunc
      .ctx <<- .rs$NewContext()
      .ctx$onInvalidate(.self$getValue)
      .firstInvocation <<- TRUE
      .isObserver <<- FALSE
    },
    setName = function(n){
      .name <<- n
    },
    getName = function() .name,
    setReactiveFunction = function(func){
      .func <<- func
    },
    setSetupFunction = function(func){
      .setupFunc <<- func
    },
    setup = function(){
      e <- try(.setupFunc(envir=.rs$envir,input=.rs$input,name=.name),silent=FALSE)
      if (!is.environment(e)) return(invisible())
      old.env <- environment(.func)
      environment(.func) <<- e
      parent.env(environment(.func)) <<- old.env
      invisible()
    },
    invalidate = function(){
      if (isObserver()){
        lapply(.ctx$.dependencies$contexts(),function(ctx){
            ctx$invalidate()
        })
      }
      .ctx$invalidate()
    },
    isObserver = function(val=FALSE){
      if (missing(val)) return(.isObserver)
      .isObserver <<- val
    },
    getValue = function(...) {
      .ctx$addDependant()

      if (.firstInvocation){
        .firstInvocation <<- FALSE
        .ctx$invalidateHint()
        .ctx$run(function() {
          #.value <<- try(.func(), silent=FALSE)
          .value <<- .func()
        })
        if (isObserver()){
          lapply(.ctx$.dependencies$contexts(),function(ctx){
              ctx$onInvalidateHint(.observerFun)
          })
          .ctx$clearInvalidatedHints()
        }
        return(invisible(.value))
      }
      if (.ctx$visited()){
        #cat('getValue id',.ctx$id,'VISITED\n')
        return(invisible(.value))
      }
      #cat('getValue id',.ctx$id,'\n')

      old.value <- .value
      .ctx$runDependencies()
      if (.ctx$isInvalidated()){
        .ctx$validate()
        .ctx$.dependencies <<- .rs$NewDependencies()
        .ctx$run(function() {
          .value <<- try(.func(), silent=FALSE)
        })
        .ctx$visited(TRUE)
      }

      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))
      if (!identical(old.value, .value))
        .ctx$invalidateDependants()

      invisible(.value)
    },
    observeWith = function(func){
      .observerFun <<- func
      .ctx$onInvalidateHint(.observerFun)
    },
    isObserver = function(){
      .isObserver <<- TRUE
    }
  )
)

ReactiveObserver <- setRefClass(
  'ReactiveObserver',
  contains = c('ReactiveObject'),
  fields = c('.rf'),
  methods = list(
    initialize = function(fun,...){
      callSuper(...)
      .rf <<- .rs$NewReactiveFunction(fun)
      .rf$isObserver(TRUE)
    },
    observeWith = function(fun){
      .rf$observeWith(fun)
    },
    run = function(){
      .rf$getValue()
    },
    invalidate = function(){
      .rf$invalidate()
    }
  )
)
