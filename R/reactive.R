ReactiveSystem <- setRefClass(
  'ReactiveSystem',
  fields = c('.currentContext','.nextId', '.pendingInvalidate','.envir',
             'setup','input','output'),
  methods = list(
    initialize = function(setup=NULL) {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingInvalidate <<- Map$new()
      .envir <<- new.env(emptyenv())
      fixEnvironment()

      if (!is.null(setup) && is.function(setup)){
        setup <<- setup
        input <<- .self$NewValues()
        output <<- Map$new()
        local({ setup(input=input,output=output) })
      }
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
    fixEnvironment = function(){
      list2env(
        list(
          .re = .self,
          reactive.function = function(x) {
            .re$NewReactiveFunction(x,.re=.re)$getValue
          }
        ),
        .envir)
    },
    addToEnvironment = function(...){
      fixEnvironment()
      list2env(
        list(...),
        .envir)
      fixEnvironment()
    },
    getEnvironment = function(){
      list2env(as.list(.envir))
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
      Dependencies$new(.re=.self)
    },
    NewContext = function(){
      ctx <- Context$new(.re=.self)
      ctx$id <- nextid()
      ctx
    },
    NewReactiveValues = function(){
      reactiveValues(ReactiveValues$new(.re=.self))
    },
    NewReactiveFunction = function(func){
      ReactiveFunction$new(func,.re=.self)
    }
  )
)

ReactiveObject <- setRefClass(
  'ReactiveObject',
  fields = list('.re'='ReactiveSystem')
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
      if (!is.null(ctx))
        .ctx <- ctx
      else if (!is.null(.re$currentContext()))
        .ctx <- .re$currentContext()
      else
        return()

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
    },
    invalidateHint = function() {
      lapply(
        .dependencies$values(),
        function(dep.ctx) {
          dep.ctx$invalidateHint()
          NULL
        })
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
      .invalidatedHint <<- FALSE
      .dependants <<- .re$NewDependencies()
      .dependencies <<- .re$NewDependencies()
      callSuper(...)
    },
    addDependant = function(){
      ctx <- .re$currentContext()
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
      .re$runWith(.self, func)
    },
    runDependencies = function(){
       # TODO: loop through all dependencies calling their run() method
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
      .re$addPendingInvalidate(.self)
      NULL
    },
    isInvalidated = function(){
      .re$isPendingInvalidate(.self)
    },
    validate = function(){
      .getReactiveSystem()$removePendingInvalidate(.self)
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

      v <- base::get(key, pos=.values, inherits=FALSE)
      v$dependants$register()

      v$val
    },
    set = function(key, value) {
      if (exists(key, where=.values, inherits=FALSE)) {
        if (identical(.values$key$val, value)) 
          return(invisible(.values$key$val))
      } else {
        assign(key,
          list(val=value,dependants=.re$NewDependencies()), 
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

reactiveValues <- function(re){
  val = list(impl=re)
  class(val) <- 'reactiveValues'
  val
}

`$.reactiveValues` <- function(x,name){
  cat('class',class(x),'\n')
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
    .ctx = 'ANY'
  ),
  methods = list(
    initialize = function(func,...) {
      .ctx <<- .re$NewContext()
      .func <<- func

      func.env <- environment(.func)
      environment(.fun) <- .re$getEnvironment()
      parent.env(environment(.fun)) <- func.env

      .ctx$onInvalidate(function() {
        .self$getValue()
      })
      callSuper(...)
    },
    getValue = function(...) {

      old.value <- .value
      old.ctx <- .ctx

      .ctx <<- .re$NewContext()
      .ctx$.hintCallbacks <<- old.ctx$.hintCallbacks
      .ctx$onInvalidate(function() {
        .self$getValue()
      })
      .ctx$runDependencies()
      if (.ctx$isInvalidated()){
         .ctx$run(function() {
           .value <<- try(.func(), silent=FALSE)
         })
      }
      old.ctx$validate()
      if (!identical(old.value, .value))
        old.ctx$invalidateDependants()

      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))
      return(.value)
    },
    observeWith = function(func){
      .ctx$onInvalidateHint(func)
    }
  )
)
