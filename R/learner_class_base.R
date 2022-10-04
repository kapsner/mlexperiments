#' @export
MLLearnerBase <- R6::R6Class( # nolint
  classname = "MLLearnerBase",
  public = list(
    cluster_export = NULL,
    metric_optimization_higher_better = NULL,
    metric_performance_higher_better = NULL,
    metric_performance_name = NULL,
    environment = -1L,
    initialize = function() {
    },
    cross_validation = function(...) {
      kwargs <- list(...)
      do.call(private$fun_optim_cv, kwargs)
    },
    fit = function(...) {
      kwargs <- list(...)
      do.call(private$fun_fit, kwargs)
    },
    predict = function(model, newdata, ncores, ...) {
      kwargs <- list(
        model = model,
        newdata = newdata,
        ncores = ncores
      )
      catch_kwargs <- list(...)
      if (length(catch_kwargs) > 0) {
        kwargs <- c(kwargs, catch_kwargs)
      }
      do.call(private$fun_predict, kwargs)
    },
    bayesian_scoring_function = function(...) {
      kwargs <- list(...)
      do.call(private$fun_bayesian_scoring_function, kwargs)
    },
    performance_metric = function(ground_truth, predictions) {
      kwargs <- list(
        ground_truth = ground_truth,
        predictions = predictions
      )
      do.call(private$fun_performance_metric, kwargs)
    }
  ),
  private = list(
    fun_optim_cv = NULL,
    fun_bayesian_scoring_function = NULL,
    fun_fit = NULL,
    fun_predict = NULL,
    fun_performance_metric = NULL
  )
)
