.config_grid_optimizer <- function(self, private) {
  stopifnot(
    is.list(private$method_helper$fold_list)
  )
  # init and configure optimizer
  optimizer <- GridOptimizer$new(learner = self$learner)
  optimizer$parameter_grid <- self$parameter_grid
  return(optimizer)
}

.config_bayesian_optimizer <- function(self, private) {
  stopifnot(
    !is.null(self$parameter_bounds),
    is.list(private$method_helper$fold_list)
  )
  # init and configure optimizer
  optimizer <- BayesianOptimizer$new(
    learner = self$learner,
    ... = self$optim_args
  )
  if (private$ncores > 1L) {
    optimizer$optim_args$parallel <- TRUE
  }
  optimizer$parameter_bounds <- self$parameter_bounds
  optimizer$parameter_grid <- private$execute_params

  return(optimizer)
}

.run_optimizer <- function(self, private, optimizer) {
  optim_results <- optimizer$execute(
    x = private$x,
    y = private$y,
    method_helper = private$method_helper,
    ncores = private$ncores,
    seed = self$seed
  )

  outlist <- .optimize_postprocessing(
    self = self,
    private = private,
    results_object = optim_results,
    metric_higher_better = optimizer$metric_higher_better
  )

  outlist <- c(outlist, private$method_helper)
  class(outlist) <- c("list", "mlexTune")
  self$results <- outlist
}
