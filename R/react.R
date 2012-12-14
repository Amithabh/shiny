Dependencies <- setRefClass(
  'Dependencies',
  fields = list(
    .dependencies = 'Map'
  ),
  methods = list(
    register = function() {
      ctx <- .getReactiveEnvironment()$currentContext()
      if (is.null(ctx))
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
    },
    validate = function(){
      lapply(
        .dependencies$values(),
        function(ctx) {
          ctx$validate()
          ctx$validateDependants()
          NULL
        }
      )
    }
  )
)

Context <- setRefClass(
  'Context',
  fields = list(
    id = 'character',
    .invalidated = 'logical',
    .invalidatedHint = 'logical',
    .callbacks = 'list',
    .hintCallbacks = 'list',
    .dependants = 'Dependencies'
  ),
  methods = list(
    initialize = function() {
      id <<- .getReactiveEnvironment()$nextId() 
      .dependants$register()
      .invalidated <<- FALSE
      .invalidatedHint <<- FALSE
    },
    addDependant = function(){
      .dependants$register()
    },
    invalidateDependants = function(){
      .dependants$invalidate()
    },
    validateDependants = function(){
      .dependants$validate()
    },
    run = function(func) {
      "Run the provided function under this context."
      env <- .getReactiveEnvironment()
      env$runWith(.self, func)
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
      if (.invalidated)
        return()
      .invalidated <<- TRUE
      .getReactiveEnvironment()$addPendingInvalidate(.self)
      NULL
    },
    validate = function(){
      "Unschedule this context for invalidation, typically done by dependencies
        so that they run first."
      if (!.invalidated)
        return()
      .invalidated <<- FALSE
      .invalidatedHint <<- FALSE
      .getReactiveEnvironment()$removePendingInvalidate(id)
      NULL
    },
    onInvalidate = function(func) {
      "Register a function to be called when this context is invalidated.
        If this context is already invalidated, the function is called
        immediately."
      if (.invalidated)
        func()
      else
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

ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  fields = c('.currentContext', '.nextId', '.pendingInvalidate'),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingInvalidate <<- Map$new()
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      return(.currentContext)
    },
    runWith = function(ctx, func) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      func()
    },
    addPendingInvalidate = function(ctx) {
      .pendingInvalidate$set(ctx$id,ctx)
    },
    removePendingInvalidate = function(id){
      .pendingInvalidate$remove(id)
    },
    pendingInvalidates = function(){
      return(.pendingInvalidates)
    },
    flush = function() {
      ctxKeys <- .pendingInvalidate$keys()
      while (length(ctxKeys) > 0) {
        ctx <- .pendingInvalidate$remove(ctxKeys[1])
        ctx$executeCallbacks()
        ctxKeys <- .pendingInvalidate$keys()
      }
    }
  )
)

.reactiveEnvironment <- ReactiveEnvironment$new()
.getReactiveEnvironment <- function() {
  .reactiveEnvironment
}

# Causes any pending invalidations to run.
flushReact <- function() {
  .getReactiveEnvironment()$flush()
}

# Retrieves the current reactive context, or errors if there is no reactive
# context active at the moment.
getCurrentContext <- function() {
  .getReactiveEnvironment()$currentContext()
}
