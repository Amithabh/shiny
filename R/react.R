ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  fields = c('.currentContext','.nextId', '.pendingInvalidate'),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingInvalidate <<- Map$new()
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
      Dependencies$new(.self)
    },
    NewContext = function(){
      ctx <- Context$new(.self)
      ctx$id <- nextid()
      ctx
    },
    NewReactiveValues = function(){
      ReactiveValues$new(.self)
    },
    NewReactiveFunction = function(func){
      ReactiveFunction$new(func,.self)
    }
  )
)

ReactiveObject <- setRefClass(
  'ReactiveObject',
  fields = list(
    .re = 'ReactiveEnvironment'
  )
)

Dependencies <- setRefClass(
  'Dependencies',
  contains = 'ReactiveObject',
  fields = list(
    .dependencies = 'Map'
  ),
  methods = list(
    initialize = function(...){
      callSuper(...)
    },
    register = function(ctx) {
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
  contains = 'ReactiveObject',
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
      .dependants <<- .re$NewDependencies()
      .dependencies <<- .re$NewDependencies()
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
      .getReactiveEnvironment()$removePendingInvalidate(.self)
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
