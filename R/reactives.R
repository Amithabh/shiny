Values <- setRefClass(
  'Values',
  fields = list(
    .values = 'environment'
  ),
  methods = list(
    initialize = function(lst=NULL) {
      .values <<- new.env(parent=emptyenv())
      if (!is.null(lst))
         mset(lst)
    },
    get = function(key) {
      if (!exists(key, where=.values, inherits=FALSE))
        return(NULL)

      v <- base::get(key, pos=.values, inherits=FALSE)
      ctx <- .getReactiveEnvironment()$currentContext()
      v$dependants$register(ctx)

      v$val
    },
    set = function(key, value) {
      if (exists(key, where=.values, inherits=FALSE)) {
        if (identical(.values$key$val, value)) 
          return(invisible(.values$key$val))
      } else {
        assign(key,
          list(val=value,dependants=Dependencies$new()), 
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

reactiveValues <- function(values=NULL){
   val <- list(impl=NULL)
   if (is.null(values) || class(values)=='list'){
     val$impl <- Values$new(values)
   } else if (class(values)=='Values'){
     val$impl <- values
   } else {
     stop("Need a list or Values object")
   }
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

Observable <- setRefClass(
  'Observable',
  fields = list(
    .re = 'ANY',
    .func = 'function',
    .value = 'ANY',
    .ctx = 'ANY'
  ),
  methods = list(
    initialize = function(func,re=NULL) {
      if(is.null(re) || class(re)!='ReactiveEnvironment')
        stop("Need a ReactiveEnvironment object")
      if (length(formals(func)) > 0)
        stop("Can't make a reactive function from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .re <<- re
      .func <<- func
    },
    getValue = function() {
      .self$.updateValue()
      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))
      return(.value)
    },
    .updateValue = function() {
      old.value <- .value
      old.ctx <- .ctx
      .ctx <<- .re$newContext()
      .ctx$onInvalidate(function() {
        .self$.updateValue()
      })
      .ctx$runDependencies()
      if (.ctx$isInvalidated()){
         .ctx$run(function() {
           .value <<- try(.func(), silent=FALSE)
         })
      }
      if (!is(old.ctx,'uninitializedField')) {
        old.ctx$validate()
        if (!identical(old.value, .value))
          old.ctx$invalidateDependants()
      }
    }
  )
)

#' Create a Reactive Function
#' 
#' Wraps a normal function to create a reactive function. Conceptually, a 
#' reactive function is a function whose result will change over time.
#' 
#' Reactive functions are functions that can read reactive values and call other
#' reactive functions. Whenever a reactive value changes, any reactive functions
#' that depended on it are marked as "invalidated" and will automatically 
#' re-execute if necessary. If a reactive function is marked as invalidated, any
#' other reactive functions that recently called it are also marked as 
#' invalidated. In this way, invalidations ripple through the functions that 
#' depend on each other.
#' 
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{Shiny tutorial} for 
#' more information about reactive functions.
#' 
#' @param x The value or function to make reactive. The function must not have 
#'   any parameters.
#' @return A reactive function. (Note that reactive functions can only be called
#'   from within other reactive functions.)
#'   
#' @export
reactive <- function(x,...) {
  UseMethod("reactive")
}
#' @S3method reactive function
reactive.function <- function(x,...) {
  return(Observable$new(x,.getReactiveEnvironment())$getValue)
}
#' @S3method reactive default
reactive.default <- function(x) {
  stop("Don't know how to make this object reactive!")
}

Observer <- setRefClass(
  'Observer',
  fields = list(
    .re = 'ANY',
    .func = 'function',
    .hintCallbacks = 'list'
  ),
  methods = list(
    initialize = function(func,re) {
      if(is.null(re) || class(re)!='ReactiveEnvironment')
        stop("Need a ReactiveEnvironment object")
      if (length(formals(func)) > 0)
        stop("Can't make an observer from a function that takes parameters; ",
             "only functions without parameters can be reactive.")
      .re <<- re
      .func <<- func

      # Defer the first running of this until flushReact is called
      ctx <- .re$newContext()
      ctx$onInvalidate(function() {
        run()
      })
      ctx$invalidate()
    },
    run = function() {
      ctx <- .re$newContext()
      ctx$onInvalidate(function() {
        run()
      })
      ctx$onInvalidateHint(function() {
        lapply(.hintCallbacks, function(func) {
          func()
          NULL
        })
      })
      ctx$run(.func)
    },
    onInvalidateHint = function(func) {
      .hintCallbacks <<- c(.hintCallbacks, func)
    }
  )
)

#' Create a reactive observer
#' 
#' Creates an observer from the given function. An observer is like a reactive 
#' function in that it can read reactive values and call reactive functions, and
#' will automatically re-execute when those dependencies change. But unlike 
#' reactive functions, it doesn't yield a result and can't be used as an input 
#' to other reactive functions. Thus, observers are only useful for their side 
#' effects (for example, performing I/O).
#' 
#' Another contrast between reactive functions and observers is their execution
#' strategy. Reactive functions use lazy evaluation; that is, when their
#' dependencies change, they don't re-execute right away but rather wait until
#' they are called by someone else. Indeed, if they are not called then they
#' will never re-execute. In contrast, observers use eager evaluation; as soon
#' as their dependencies change, they schedule themselves to re-execute.
#' 
#' @param func The function to observe. It must not have any parameters. Any 
#'   return value from this function will be ignored.
#'   
#' @export
observe <- function(func) {
  Observer$new(func)
  invisible()
}

#' Timer
#' 
#' Creates a reactive timer with the given interval. A reactive timer is like a 
#' reactive value, except reactive values are triggered when they are set, while
#' reactive timers are triggered simply by the passage of time.
#' 
#' \link[=reactive]{Reactive functions} and observers that want to be 
#' invalidated by the timer need to call the timer function that 
#' \code{reactiveTimer} returns, even if the current time value is not actually 
#' needed.
#' 
#' See \code{\link{invalidateLater}} as a safer and simpler alternative.
#' 
#' @param intervalMs How often to fire, in milliseconds
#' @return A no-parameter function that can be called from a reactive context, 
#'   in order to cause that context to be invalidated the next time the timer 
#'   interval elapses. Calling the returned function also happens to yield the 
#'   current time (as in \code{\link{Sys.time}}).
#' @seealso invalidateLater
#' @export
reactiveTimer <- function(intervalMs=1000) {
  dependencies <- Map$new()
  timerCallbacks$schedule(intervalMs, function() {
    timerCallbacks$schedule(intervalMs, sys.function())
    lapply(
      dependencies$values(),
      function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
  })
  return(function() {
    ctx <- .getReactiveEnvironment()$currentContext()
    if (!dependencies$containsKey(ctx$id)) {
      dependencies$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependencies$remove(ctx$id)
      })
    }
    return(Sys.time())
  })
}

#' Scheduled Invalidation
#' 
#' Schedules the current reactive context to be invalidated in the given number 
#' of milliseconds.
#' @param millis Approximate milliseconds to wait before invalidating the
#'   current reactive context.
#' @export
invalidateLater <- function(millis) {
  ctx <- .getReactiveEnvironment()$currentContext()
  timerCallbacks$schedule(millis, function() {
    ctx$invalidate()
  })
  invisible()
}
