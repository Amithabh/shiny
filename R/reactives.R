ReactiveValues <- setRefClass(
  'ReactiveValues',
  contains = 'ReactiveObject',
  fields = list(
    .values = 'environment'
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
    },
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
  val$impl <- ReactiveValues$new(re)
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
  contains = 'ReactiveObject',
  fields = list(
    .func = 'function',
    .value = 'ANY',
    .ctx = 'ANY'
  ),
  methods = list(
    initialize = function(func,...) {
      callSuper(...)
      if (length(formals(func)) > 0)
        stop("Can't make a reactive function from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .ctx <<- .re$NewContext()
      .func <<- func
      .ctx$onInvalidate(function() {
        .self$getValue()
      })
    },
    getValue = function(...) {
      old.value <- .value
      old.ctx <- .ctx
      .ctx <<- .re$NewContext()
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
    callWith = function(...){
    },
    observeWith = function(func){
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
