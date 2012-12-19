ReactiveSystem <- setRefClass(
  'ReactiveSystem',
  fields = c('.currentContext','.nextId', '.pendingInvalidate','envir','input','output'),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingInvalidate <<- Map$new()
      envir <<- NULL
      input <<- NULL
      output <<- NULL
    },
    currentContext = function() {
      .currentContext
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    addPendingInvalidate = function(ctx) {
      .pendingInvalidate$set(ctx$id,ctx)
    },
    removePendingInvalidate = function(ctx){
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
      ctxKeys <- .pendingInvalidate$keys()
      while (length(ctxKeys) > 0) {
        ctx <- .pendingInvalidate$get(ctxKeys[1])
        ctx$executeCallbacks()
        .pendingInvalidate$remove(ctxKeys[1])
        ctxKeys <- .pendingInvalidate$keys()
      }
    },
    NewDependencies = function(){
      Dependencies$new(.rs=.self)
    },
    NewContext = function(){
      ctx <- Context$new(.rs=.self)
      ctx$id <- nextId()
      ctx
    },
    NewReactiveValues = function(){
      S3ReactiveValues(ReactiveValues$new(.rs=.self))
    },
    NewReactiveFunction = function(func){
      ReactiveFunction$new(func,.rs=.self)
    },
    setupWith = function(setupFun,envirClass=ReactiveEnvironment){
      if (is.null(input))
        input <<- .self$NewReactiveValues()
      if (is.null(output))
        output <<- S3Map(Map$new())
      if (is.object(envirClass))
        klass <- envirClass$className
      else if (is.character(envirClass))
        klass <- envirClass
      envir <<- local({setRefClass(
        paste(klass,gsub('-','',as.character(rnorm(1))),sep=''),
        contains=c(klass),
        methods=list(setup=setupFun)
      )$new(.rs=.self)})
      envir$setup(input=input,output=output)
    },
    show = function()cat('A Reactive System\n')
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
    reactive = function(x){
      .rs$NewReactiveFunction(x)$getValue
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
        stop("No currentContext!\n")
      }
      if (!.dependencies$containsKey(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependencies$remove(ctx$id)
        })
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
          ctx$executCallbacks()
          NULL
        }
      )
      invisible()
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
    .dependencies = 'ANY'
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
      .invalidatedHint <<- FALSE
      .dependants <<- .rs$NewDependencies()
      .dependencies <<- .rs$NewDependencies()
    },
    addDependant = function(){
      .dependants$register(ctx)
      ctx$addDependency(.self)
    },
    addDependency = function(ctx){
      .dependencies$register(ctx)
    },
    invalidateDependants = function(){
      .dependants$invalidate()
    },
    run = function(func) {
      "Run the provided function under this context."
      .rs$runWith(.self, func)
    },
    runDependencies = function(){
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
      lapply(.hintCallbacks, function(func) {
        func()
      })
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
      if (.invalidatedHint)
        func()
      else
        .hintCallbacks <<- c(.hintCallbacks, func)
    },
    executeCallbacks = function() {
      "For internal use only."
      lapply(.callbacks, function(func) {
        withCallingHandlers({
          func()
        }, warning = function(e) {
          # TODO: Callbacks in app
        }, error = function(e) {
          # TODO: Callbacks in app
        })
      })
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
        if (identical(.values$key$val, value)) 
          return(invisible(.values$key$val))
      } else {
        assign(key,
          list(val=value,dependants=.rs$NewDependencies()), 
          pos=.values, inherits=FALSE)
      }

      .values[[key]]$val <<- value
      .values[[key]]$dependants$invalidate()

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
  x[['impl']]$get(name)
}

`$<-.reactiveValues` <- function(x,name,value){
  x[['impl']]$set(name,value)
  x
}

`<-.reactiveValues` <- function(x,value){
   if(!is('list',value)) stop("Value not a list!")
   invisible(x$mset(value))
}

as.list.reactiveValues <- function(x, ...) {
  x[['impl']]$mget()
}

names.reactiveValues <- function(x) {
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
    .func = 'function',
    .value = 'ANY',
    .ctx = 'ANY',
    .firstInvocation = 'logical'
  ),
  methods = list(
    initialize = function(func,...) {
      callSuper(...)
      .func <<- func
      .ctx <<- .rs$NewContext()
      .ctx$onInvalidate(function() {
        .self$getValue()
      })
      .firstInvocation <<- TRUE
    },
    getValue = function(...) {

      if (.firstInvocation){
        .firstInvocation <<- FALSE
         .ctx$run(function() {
           #.value <<- try(.func(), silent=FALSE)
           .value <<- .func()
         })
        return(invisible(.value))
      }

      old.value <- .value
      .ctx$runDependencies()
      if (.ctx$isInvalidated()){
          .ctx$validate()
          .ctx <<- .rs$NewContext()
          .ctx$onInvalidate(function() {
            .self$getValue()
          })
         .ctx$run(function() {
           .value <<- try(.func(), silent=FALSE)
         })
      }
      if (!identical(old.value, .value))
        .ctx$invalidateDependants()

      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))

      invisible(.value)
    },
    observeWith = function(func){
      .ctx$onInvalidateHint(func)
    }
  )
)
