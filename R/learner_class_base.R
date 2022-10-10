#' @export
MLLearnerBase <- R6::R6Class( # nolint
  classname = "MLLearnerBase",
  public = list(
    cluster_export = NULL,
    metric_optimization_higher_better = NULL,
    performance_metric_name = NULL,
    environment = -1L,
    initialize = function(
      metric_optimization_higher_better # nolint
      ) {
      stopifnot(
        is.logical(metric_optimization_higher_better) ||
          is.null(metric_optimization_higher_better)
      )
      self$metric_optimization_higher_better <-
        metric_optimization_higher_better
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
      kwargs <- kdry::list.append(kwargs, catch_kwargs)
      do.call(private$fun_predict, kwargs)
    },
    bayesian_scoring_function = function(...) {
      kwargs <- list(...)
      args <- .method_params_refactor(
        kwargs,
        method_helper
      )
      res <- do.call(private$fun_bayesian_scoring_function, args)

      # take care of transforming results in case higher-better = FALSE
      # --> bayesOpt tries to maximize the metric, so it is required to
      # reverse score
      if (isFALSE(self$metric_optimization_higher_better)) {
        res$Score <- as.numeric(I(res$Score * -1L))
      }
      return(res)
    }
  ),
  private = list(
    fun_optim_cv = NULL,
    fun_bayesian_scoring_function = NULL,
    fun_fit = NULL,
    fun_predict = NULL
  )
)
