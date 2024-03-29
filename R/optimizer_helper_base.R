.config_grid_optimizer <- function(self, private) {
  stopifnot(
    "`fold_list` is not a list" = is.list(private$method_helper$fold_list)
  )
  # init and configure optimizer
  optimizer <- GridOptimizer$new(
    learner = self$learner,
    seed = private$seed,
    ncores = private$ncores
  )
  return(optimizer)
}

.config_bayesian_optimizer <- function(self, private) {
  stopifnot(
    "`parameter_bounds` must not be empty" = !is.null(self$parameter_bounds),
    "`fold_list` must be a list" = is.list(private$method_helper$fold_list)
  )
  # init and configure optimizer
  optimizer <- BayesianOptimizer$new(
    learner = self$learner,
    seed = private$seed,
    ncores = private$ncores,
    ... = self$optim_args
  )
  if (private$ncores > 1L) {
    optimizer$optim_args$parallel <- TRUE
  }
  optimizer$parameter_bounds <- self$parameter_bounds

  return(optimizer)
}

.run_optimizer <- function(self, private, optimizer) {
  optim_results <- optimizer$execute(
    x = private$x,
    y = private$y,
    method_helper = private$method_helper
  )

  outlist <- .optimize_postprocessing(
    self = self,
    private = private,
    results_object = optim_results,
    metric_higher_better = optimizer$learner$metric_optimization_higher_better
  )

  outlist <- kdry::list.append(outlist, private$method_helper)
  class(outlist) <- c("list", "mlexTune")
  self$results <- outlist
}
