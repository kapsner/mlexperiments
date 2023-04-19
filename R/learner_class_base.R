#' @title R6 Class to construct learners
#'
#' @description
#' The `MLLearnerBase` class is used to construct a learner object that can be
#'   used with the experiment classes from the `mlexperiments` package. It is
#'   thought to serve as a class to inherit from when creating new learners.
#'
#' @details
#' The learner class exposes 4 methods that can be defined:
#' * `$fit` A wrapper around the private function `fun_fit`, which needs to
#'   be defined for every learner. The return value of this function is the
#'   fitted model.
#' * `$predict` A wrapper around the private function `fun_predict`,
#'   which needs to be defined for every learner. The function must accept the
#'   three arguments `model`, `newdata`, and `ncores` and is a wrapper around
#'   the respective learner's predict-function. In order to allow the passing of
#'   further arguments, the ellipsis (`...`) can be used. The function should
#'   return the prediction results.
#' * `$cross_validation` A wrapper around the private function
#'   `fun_optim_cv`, which needs to be defined when hyperparameters should be
#'   optimized with a grid search (required for use with
#'   [mlexperiments::MLTuneParameters], and [mlexperiments::MLNestedCV]).
#' * `$bayesian_scoring_function` A wrapper around the private function
#'   `fun_bayesian_scoring_function`, which needs to be defined when
#'   hyperparameters should be optimized with a Bayesian process (required for
#'   use with [mlexperiments::MLTuneParameters], and
#'   [mlexperiments::MLNestedCV]).
#'
#' For further details please refer to the package's vignette.
#'
#' @examples
#' MLLearnerBase$new(metric_optimization_higher_better = FALSE)
#'
#' @export
#'
MLLearnerBase <- R6::R6Class( # nolint
  classname = "MLLearnerBase",
  public = list(
    #' @field cluster_export A character vector defining the (internal)
    #'   functions that need to be exported to the parallelization cluster.
    #'   This is only required when performing a Bayesian hyperparameter
    #'   optimization. See also [parallel::clusterExport()].
    cluster_export = NULL,

    #' @field metric_optimization_higher_better A logical. Defines the direction
    #'  of the optimization metric used throughout the hyperparameter
    #'  optimization. This field is set automatically during the initialization
    #'  of the `MLLearnerBase` object. Its purpose is to make it accessible by
    #'  the evaluation functions from [mlexperiments::MLTuneParameters].
    metric_optimization_higher_better = NULL,

    #' @field environment The environment in which to search for the functions
    #'   of the learner (default: `-1L`).
    environment = -1L,

    #' @description
    #' Create a new `MLLearnerBase` object.
    #'
    #' @param metric_optimization_higher_better A logical. Defines the direction
    #'  of the optimization metric used throughout the hyperparameter
    #'  optimization.
    #'
    #' @return A new `MLLearnerBase` R6 object.
    #'
    #' @examples
    #' MLLearnerBase$new(metric_optimization_higher_better = FALSE)
    #'
    initialize = function(
      metric_optimization_higher_better # nolint
      ) {
      stopifnot(
        "`metric_optimization_higher_better` must be a boolean value or \
        `NULL`" = is.logical(metric_optimization_higher_better) ||
          is.null(metric_optimization_higher_better)
      )
      self$metric_optimization_higher_better <-
        metric_optimization_higher_better
    },

    #' @description
    #' Perform a cross-validation with an `MLLearnerBase`.
    #'
    #' @details
    #' A wrapper around the private function `fun_optim_cv`, which needs to be
    #'   defined when hyperparameters should be optimized with a grid search
    #'   (required for use with [mlexperiments::MLTuneParameters], and
    #'   [mlexperiments::MLNestedCV].
    #'   However, the function should be never executed directly but by the
    #'   respective experiment wrappers [mlexperiments::MLTuneParameters], and
    #'   [mlexperiments::MLNestedCV].
    #'   For further details please refer to the package's vignette.
    #'
    #' @param ... Arguments to be passed to the learner's cross-validation
    #'   function.
    #'
    #' @return The fitted model.
    #'
    #' @seealso [mlexperiments::MLTuneParameters],
    #'   [mlexperiments::MLCrossValidation], and
    #'   [mlexperiments::MLNestedCV]
    #'
    #' @examples
    #' \dontrun{
    #' learner <- MLLearnerBase$new(metric_optimization_higher_better = FALSE)
    #' learner$cross_validation()
    #' }
    #'
    cross_validation = function(...) {
      kwargs <- list(...)
      do.call(private$fun_optim_cv, kwargs)
    },

    #' @description
    #' Fit a `MLLearnerBase` object.
    #'
    #' @details
    #' A wrapper around the private function `fun_fit`, which needs to be
    #'   defined for every learner. The return value of this function is the
    #'   fitted model.
    #'   However, the function should be never executed directly but by the
    #'   respective experiment wrappers [mlexperiments::MLTuneParameters],
    #'   [mlexperiments::MLCrossValidation], and
    #'   [mlexperiments::MLNestedCV].
    #'   For further details please refer to the package's vignette.
    #'
    #' @param ... Arguments to be passed to the learner's fitting function.
    #'
    #' @return The fitted model.
    #'
    #' @seealso [mlexperiments::MLTuneParameters],
    #'   [mlexperiments::MLCrossValidation], and
    #'   [mlexperiments::MLNestedCV]
    #'
    #' @examples
    #' \dontrun{
    #' learner <- MLLearnerBase$new(metric_optimization_higher_better = FALSE)
    #' learner$fit()
    #' }
    #'
    fit = function(...) {
      kwargs <- list(...)
      do.call(private$fun_fit, kwargs)
    },

    #' @description
    #' Make predictions from a fitted `MLLearnerBase` object.
    #'
    #' @details
    #' A wrapper around the private function `fun_predict`, which needs to be
    #'   defined for every learner. The function must accept the three arguments
    #'   `model`, `newdata`, and `ncores` and is a wrapper around the respective
    #'   learner's predict-function. In order to allow the passing of further
    #'   arguments, the ellipsis (`...`) can be used. The function should
    #'   return the prediction results.
    #'   However, the function should be never executed directly but by the
    #'   respective experiment wrappers [mlexperiments::MLTuneParameters],
    #'   [mlexperiments::MLCrossValidation], and
    #'   [mlexperiments::MLNestedCV].
    #'   For further details please refer to the package's vignette.
    #'
    #' @param model A fitted model of the learner (as returned by
    #'   `MLLearnerBase$fit()`).
    #' @param newdata The new data for which predictions should be made using
    #'   the `model`.
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #' @param ... Further arguments to be passed to the learner's predict
    #'   function.
    #'
    #' @return The predictions for `newdata`.
    #'
    #' @seealso [mlexperiments::MLTuneParameters],
    #'   [mlexperiments::MLCrossValidation], and
    #'   [mlexperiments::MLNestedCV]
    #'
    #' @examples
    #' \dontrun{
    #' learner <- MLLearnerBase$new(metric_optimization_higher_better = FALSE)
    #' learner$fit()
    #' learner$predict()
    #' }
    #'
    predict = function(model, newdata, ncores = -1L, ...) {
      if (ncores < 0) {
        ncores <- kdry::pch_check_available_cores()
      }
      kwargs <- list(
        model = model,
        newdata = newdata,
        ncores = ncores
      )
      catch_kwargs <- list(...)
      kwargs <- kdry::list.append(kwargs, catch_kwargs)
      do.call(private$fun_predict, kwargs)
    },

    #' @description
    #' Perform a Bayesian hyperparameter optimization with an `MLLearnerBase`.
    #'
    #' @details
    #' A wrapper around the private function `fun_bayesian_scoring_function`,
    #'   which needs to be defined when hyperparameters should be optimized with
    #'   a Bayesian process (required for use with
    #'   [mlexperiments::MLTuneParameters], and [mlexperiments::MLNestedCV].
    #'   However, the function should be never executed directly but by the
    #'   respective experiment wrappers [mlexperiments::MLTuneParameters], and
    #'   [mlexperiments::MLNestedCV].
    #'   For further details please refer to the package's vignette.
    #'
    #' @param ... Arguments to be passed to the learner's Bayesian scoring
    #'   function.
    #'
    #' @return The results of the Bayesian scoring.
    #'
    #' @seealso [ParBayesianOptimization::bayesOpt()],
    #'   [mlexperiments::MLTuneParameters], and [mlexperiments::MLNestedCV]
    #'
    #' @examples
    #' \dontrun{
    #' learner <- MLLearnerBase$new(metric_optimization_higher_better = FALSE)
    #' learner$bayesian_scoring_function()
    #' }
    #'
    bayesian_scoring_function = function(...) {
      kwargs <- list(...)
      args <- .method_params_refactor(
        kwargs,
        method_helper
      )
      res <- do.call(private$fun_bayesian_scoring_function, args)

      # take care of transforming results in case higher-better = FALSE
      # --> bayesOpt tries to maximize the metric, so it is required to
      # inverse score
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
