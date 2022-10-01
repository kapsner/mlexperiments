BayesianOptimizer <- R6::R6Class( # nolint
  inherit = BaseOptimizer,
  classname = "BayesianOptimizer",
  public = list(
    #' @field optim_args A list with the arguments that are passed to
    #'   \code{ParBayesianOptimization::bayesOpt}
    optim_args = NULL,
    parameter_bounds = NULL,
    initialize = function(learner, seed, ncores, ...) {
      if (!requireNamespace("ParBayesianOptimization", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"ParBayesianOptimization\" must be installed to use ",
            "'strategy = \"bayesian\"."
          ),
          call. = FALSE
        )
      }
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      private$strategy <- "bayesian"
      kwargs <- .argument_catcher(...)

      default_args <- formals(ParBayesianOptimization::bayesOpt)
      self$optim_args <- default_args[!sapply(default_args, is.symbol)]

      # update arguments
      if (length(kwargs) > 0) {
        self$optim_args <- .update_default_arguments(
          new = kwargs,
          default = self$optim_args
        )
      }
    }
  )
)
