BayesianOptimizer <- R6::R6Class( # nolint
  inherit = BaseOptimizer,
  classname = "BayesianOptimizer",
  public = list(
    #' @field optim_args A list with the arguments that are passed to
    #'   \code{rBayesianOptimization::BayesianOptimization}
    optim_args = NULL,
    parameter_bounds = NULL,
    initialize = function(learner, seed, ncores, ...) {
      if (!requireNamespace("rBayesianOptimization", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"rBayesianOptimization\" must be installed to use ",
            "'strategy = \"bayesian\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      private$strategy <- "bayesian"
      kwargs <- kdry::misc_argument_catcher(...)

      default_args <- formals(rBayesianOptimization::BayesianOptimization)
      self$optim_args <- default_args[!sapply(default_args, is.symbol)]
      self$optim_args["n_iter"] <- 3

      # update arguments
      if (length(kwargs) > 0) {
        self$optim_args <- kdry::list.update(
          main_list = self$optim_args,
          new_list = kwargs
        )
      }
    }
  )
)
