#' @title metric
#'
#' @description Returns a metric function which can be used for the experiments
#'   (especially the cross-validation experiments) to compute the performance.
#'
#' @details
#' This function is a utility function to select performance metrics from the
#' `mlr3measures` R package and to reformat them into a form that is required
#' by the `mlexperiments` R package. For `mlexperiments` it is required that
#' a metric function takes the two arguments `ground_truth`, and `predictions`,
#' as well as additional names arguments that are necessary to compute the
#' performance, which are provided via the ellipsis argument (...).
#' When using the performance metric with an experiment of class
#' `"MLCrossValidation"`, such arguments can be defined as a list provided to
#' the field `performance_metric_args` of the R6 class.
#' The main purpose of `mlexperiments::metric()` is convenience and to
#' re-use already existing implementations of the metrics. However, custom
#' functions can be provided easily to compute the performance of the
#' experiments, simply by providing a function that takes the above mentioned
#' arguments and returns one performance metric value.
#'
#' @param name A metric name. Accepted names are the names of the metric
#'   function exported from the `mlr3measures` R package.
#'
#' @return Returns a function that can be used as function to calculate the
#'   performance metric throughout the experiments.
#'
#' @examples
#' metric("auc")
#'
#' @export
#'
metric <- function(name) {
  stopifnot(is.character(name) && length(name) == 1L)
  if (!requireNamespace("mlr3measures", quietly = TRUE)) {
    stop(
      paste0(
        "Package \"mlr3measures\" must be installed to use ",
        "function 'metric()'."
      ),
      call. = FALSE
    )
  }
  stopifnot(
    is.function(
      FUN <- utils::getFromNamespace(x = name, ns = "mlr3measures") # nolint
    )
  )

  fun_name <- paste0("mlr3measures::", name)

  default_args <- formals(FUN)
  response_name <- names(default_args)[2]
  stopifnot(response_name %in% c("response", "prob"))

  # compose function body
  fun_body <- paste0(
    "args <- list(truth = ground_truth, ", response_name, " = predictions) \n",
    "if (length(kwargs) > 0L) {\nargs <- c(args, kwargs)\n}\n",
    "return(do.call(", fun_name, ", args))"
  )

  fun <- paste0(
    "function(ground_truth, predictions, ...) {\n",
    "kwargs <- list(...)\n",
    #fun_body_pre,
    fun_body,
    "\n}"
  )
  return(eval(parse(text = fun)))
}


#' @title metric_types_helper
#'
#' @description Prepares the data to be conform with the requirements of
#'   the metrics from `mlr3measures`.
#'
#' @param FUN A metric function, created with [mlexperiments::metric()].
#' @param y The outcome vector.
#' @param perf_args A list. The arguments to call the metric function with.
#'
#' @details
#' The `mlr3measures` R package makes some restrictions on the data type of
#'   the ground truth and the predictions, depending on the metric, i.e. the
#'   type of the task (regression or classification).
#'   Thus, it is necessary to convert the inputs to the metric function
#'   accordingly, which is done with this helper function.
#'
#' @return Returns the calculated performance measure.
#'
#' @examples
#' set.seed(123)
#' ground_truth <- sample(0:1, 100, replace = TRUE)
#' predictions <- sample(0:1, 100, replace = TRUE)
#' FUN <- metric("acc")
#'
#' perf_args <- list(
#'   ground_truth = ground_truth,
#'   predictions = predictions
#' )
#'
#' metric_types_helper(
#'   FUN = FUN,
#'   y = ground_truth,
#'   perf_args = perf_args
#' )
#'
#' @export
#'
metric_types_helper <- function(FUN, y, perf_args) { # nolint
  stopifnot(is.function(FUN), is.list(perf_args),
            all(c("ground_truth", "predictions") %in% names(perf_args)))
  # note that this is very specific to the mlr3measures package
  tryCatch(
    expr = {
      return(do.call(FUN, perf_args))
    }, error = function(e) {
      if (!is.factor(y)) {
        y <- factor(y)
      }
      lvls <- levels(y)
      error <- TRUE
      if (grepl(
        pattern = "Assertion on 'truth' failed: Must be of type 'factor'",
        x = e
      )) {
        # convert to factor
        perf_args$ground_truth <- factor(
          x = perf_args$ground_truth,
          levels = lvls
        )
        error <- FALSE
      } else if (grepl(
        pattern = "Assertion on 'response' failed: Must be of type 'factor'",
        x = e
      )) {
        perf_args$predictions <- factor(
          x = perf_args$predictions,
          levels = lvls
        )
        error <- FALSE
      }

      if (isFALSE(error)) {
        # recursive execution
        args <- list(
          FUN = FUN,
          y = y,
          perf_args = perf_args
        )
        return(do.call(metric_types_helper, args))
      } else {
        stop(e)
      }
    }
  )
}
