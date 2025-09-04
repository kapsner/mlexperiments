#' @title metric
#'
#' @description Returns a metric function which can be used for the experiments
#'   (especially the cross-validation experiments) to compute the performance.
#'
#' @details
#' This function is a utility function to select performance metrics from the
#' `measures` R package and to reformat them into a form that is required
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
#'   function exported from the `measures` R package.
#'
#' @return Returns a function that can be used as function to calculate the
#'   performance metric throughout the experiments.
#'
#' @examples
#' metric("AUC_roc")
#'
#' @export
#'
metric <- function(name) {
  stopifnot(
    "`name` must be a character of length() == 1" =
      is.character(name) && length(name) == 1L
  )
  if (!requireNamespace("measures", quietly = TRUE)) {
    stop(
      paste0(
        "Package \"measures\" must be installed to use ",
        "function 'metric()'."
      ),
      call. = FALSE
    )
  }
  stopifnot(
    "`name` is not a function exported from R package {measures}" =
      is.function(
      utils::getFromNamespace(x = name, ns = "measures")
    )
  )
  FUN <- utils::getFromNamespace(x = name, ns = "measures") # nolint

  fun_name <- paste0("measures::", name)

  # get first two default-arguments
  default_args <- formals(FUN)
  default_args_names <- names(default_args)
  first_two_default_args <- default_args_names[1:2]
  if ("response" %in% first_two_default_args) {
    response_name <- "response"
  } else {
    response_name <- "probabilities"
  }

  # compose function body
  fun_body <- paste0(
    "args <- list(\n",
    "    truth = ground_truth,\n",
    "    ", response_name, " = predictions\n",
    ") \n",
    "fun_default_args <- c(",
    paste0("\"", default_args_names, collapse = "\", "), "\")\n",
    "fun_default_args <- setdiff(fun_default_args, \"...\")\n",
    "if (length(kwargs) > 0L) {\n",
    "    valid_kwargs_names <- intersect(fun_default_args, names(kwargs))\n",
    "    kwargs <- kwargs[which(names(kwargs) %in% valid_kwargs_names)]\n",
    "    if (length(kwargs) > 0L) {\n",
    "        args <- c(args, kwargs)\n}\n}\n",
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
#'   the metrics from `measures`.
#'
#' @param FUN A metric function, created with [mlexperiments::metric()].
#' @param y The outcome vector.
#' @param perf_args A list. The arguments to call the metric function with.
#'
#' @details
#' The `measures` R package makes some restrictions on the data type of
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
#' FUN <- metric("accuracy")
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
  stopifnot(
    "`FUN` must be a function" = is.function(FUN),
    "`perf_args` must be a list" = is.list(perf_args),
    "`perf_args` must contain named elements `ground_truth` and `predictions`" =
      all(c("ground_truth", "predictions") %in% names(perf_args)))
  # note that this is very specific to the measures package
  if (!requireNamespace("measures", quietly = TRUE)) {
    stop(
      paste0(
        "Package \"measures\" must be installed to use ",
        "function 'metric_types_helper()'."
      ),
      call. = FALSE
    )
  }

  regression_metrics <- c("MSE", "MAE", "MAPE", "RMSE", "RMSLE", "SSE", "RSQ")
  classification_metrics <- c(
    "AUC", "F1", "TPR", "TNR", "PPV", "NPV",
    "FNR", "FPR", "ACC", "BER", "BAC", "Brier", "multiclass.Brier"
  )

  # function name
  pat <- ".*measures::(.*),.*"
  fun_line <- grep(
    pattern = pat,
    x = deparse(FUN),
    value = TRUE
  )
  fun_name <- gsub(pattern = pat, replacement = "\\1", x = fun_line)

  # fix binary metrics here
  # logic for conversion of probabilities to classes in case of binary
  # classification
  error <- FALSE
  if (!is.factor(y) && fun_name %in% .binary_metrics() &&
      !(fun_name %in% .binary_metrics_probs()) &&
      (min(perf_args$predictions) >= 0 &&
       max(perf_args$predictions) <= 1)) {
    if (!is.factor(y)) {
      y <- factor(y)
    }
    lvls <- levels(y)
    if ("positive" %in% names(perf_args)) {
      val_positive <- perf_args$positive
      val_negative <- setdiff(lvls, val_positive)
    } else {
      if ("0" %in% lvls && "1" %in% lvls) {
        val_positive <- "1"
        val_negative <- "0"
      } else {
        stop("Argument 'pos_level' is missing.")
      }
    }

    perf_args$predictions <- ifelse(
      test = perf_args$predictions > 0.5,
      yes = val_positive,
      no = val_negative
    )

    perf_args$predictions <- factor(
      x = perf_args$predictions,
      levels = lvls
    )
    if (!any(is.na(perf_args$predictions))) {
      error <- FALSE
    }
  }


  tryCatch(
    expr = {
      if (isTRUE(error)) {
        errorCondition("An error happend preparing response for binary metric.")
      }
      return(do.call(FUN, perf_args))
    }, error = function(e) {

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
        pattern = paste0(
          "Assertion on 'response' failed: Must have length ",
          "\\d+, but has length \\d+\\."
        ),
        x = e
      )) {
        msg <- paste0(
          "An error occurred... Try to use 'predict_args <- list(",
          "reshape = TRUE)'"
        )
        stop(paste0(msg, "\n", e))
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

.metric_from_char <- function(metric_vector) {
  sapply(
    X = metric_vector,
    FUN = function(x) {
      metric(x)
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
}
