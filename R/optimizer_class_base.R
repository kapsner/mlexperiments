BaseOptimizer <- R6::R6Class( # nolint
  inherit = MLBase,
  classname = "BaseOptimizer",
  public = list(
    parameter_grid = NULL,
    metric_optimization_higher_better = NULL,
    initialize = function(learner, seed, ncores) {
      stopifnot(R6::is.R6Class(learner))
      super$initialize(seed = seed, ncores = ncores)
      private$learner <- learner$new()
      self$metric_optimization_higher_better <-
        private$learner$metric_optimization_higher_better
    },
    execute = function(x, y, method_helper, ncores, seed) {
      if (is.null(method_helper$execute_params$parameter_grid)) {
        if (is.null(private$method_helper$execute_params$parameter_grid)) {
          .organize_parameter_grid(self, private)
        }
        method_helper$execute_params <- private$method_helper$execute_params
      }
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
    strategy = NULL,
    method_helper = NULL
  )
)
