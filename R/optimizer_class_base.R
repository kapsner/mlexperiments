BaseOptimizer <- R6::R6Class( # nolint
  inherit = MLBase,
  classname = "BaseOptimizer",
  public = list(
    parameter_grid = NULL,
    metric_higher_better = NULL,
    initialize = function(learner, seed, ncores) {
      stopifnot(R6::is.R6Class(learner))
      super$initialize(seed = seed, ncores = ncores)
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
          method_helper = method_helper
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
