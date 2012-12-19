# TESTS
# Simple set/get
# Simple remove
# Simple containsKey
# Simple keys
# Simple values
# Simple clear
# Get of unknown key returns NULL
# Remove of unknown key does nothing
# Setting a key twice always results in last-one-wins
# /TESTS
Map <- setRefClass(
  'Map',
  fields = list(
    .env = 'environment'
  ),
  methods = list(
    initialize = function() {
      .env <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      if (.self$containsKey(key))
        return(base::get(key, pos=.env, inherits=FALSE))
      else
        return(NULL)
    },
    set = function(key, value) {
      assign(key, value, pos=.env, inherits=FALSE)
      invisible(value)
    },
    mset = function(...) {
      args <- list(...)
      for (key in names(args))
        set(key, args[[key]])
      invisible(args)
    },
    mget = function() {
      lstNames <- keys()
      lst <- lapply(lstNames,
          function(name) {
            .self$get(name)
      })
      names(lst) <- lstNames
      lst
    },
    remove = function(key) {
      if (.self$containsKey(key)) {
        result <- .self$get(key)
        rm(list = key, pos=.env, inherits=FALSE)
        return(result)
      }
      return(NULL)
    },
    containsKey = function(key) {
      exists(key, where=.env, inherits=FALSE)
    },
    keys = function() {
      ls(envir=.env, all.names=TRUE)
    },
    values = function() {
      base::mget(.self$keys(), envir=.env, inherits=FALSE)
    },
    clear = function() {
      .env <<- new.env(parent=emptyenv())
      invisible(NULL)
    },
    size = function() {
      length(.env)
    }
  )
)

`[.Map` <- function(map, name) {
  map$get(name)
}

`[<-.Map` <- function(map, name, value) {
  map$set(name, value)
  return(map)
}

as.list.Map <- function(map) {
  map$mget()
}

length.Map <- function(map) {
  map$size()
}

S3Map <- function(map){
  val <- list(impl=map)
  class(val) <- 'S3map'
  val
}

`$.S3map` <- function(x,name){
  x[['impl']]$get(name)
}

`$<-.S3map` <- function(x,name,value){
  x[['impl']]$set(name,value)
  x
}

`<-.S3map` <- function(x,value){
   if(!is('list',value)) stop("Value not a list!")
   invisible(x[['impl']]$mset(value))
   x
}

names.S3map <- function(x) {
  x[['impl']]$keys()
}

as.list.S3map <- function(map) {
  map[['impl']]$mget()
}

print.S3map <- function(x,...) print(as.list(x),...)
