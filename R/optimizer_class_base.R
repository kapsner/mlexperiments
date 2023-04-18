BaseOptimizer <- R6::R6Class( # nolint
  inherit = MLBase,
  classname = "BaseOptimizer",
  public = list(
    parameter_grid = NULL,
    learner = NULL,
    initialize = function(learner, seed, ncores) {
      stopifnot("`learner` must be an R6 class" = R6::is.R6(learner))
      super$initialize(seed = seed, ncores = ncores)
      self$learner <- learner
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
    strategy = NULL,
    method_helper = NULL
  )
)
