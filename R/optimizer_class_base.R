BaseOptimizer <- R6::R6Class( # nolint
  classname = "BaseOptimizer",
  public = list(
    parameter_grid = NULL,
    results = NULL,
    metric_higher_better = NULL,
    initialize = function(learner) {
      stopifnot(R6::is.R6Class(learner))
      private$learner <- learner$new()
      self$metric_higher_better <- private$learner$metric_higher_better
      private$method <- learner$classname
    },
    execute = function(x, y, method_helper, ncores, seed) {
      FUN <- switch( # nolint
        EXPR = private$strategy,
        "grid" = .grid_optimize,
        "bayesian" = .bayesian_optimize
      )
      optim_results <- do.call(
        what = FUN,
        args = list(
          self = self,
          private = private,
          x = x,
          y = y,
          seed = seed,
          method_helper = method_helper,
          ncores = ncores
        )
      )
      self$results <- optim_results
      return(self$results)
    }
  ),
  private = list(
    learner = NULL,
    method = NULL,
    strategy = NULL
  )
)
