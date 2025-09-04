#' @title performance
#'
#' @description Calculate performance measures from the predictions results.
#'
#' @param object An R6 object of class `"MLCrossValidation"` for which the
#'   performance should be computed.
#' @param prediction_results An object of class `"mlexPredictions"` (the output
#'   of the function [mlexperiments::predictions()]).
#' @param y_ground_truth A vector with the ground truth of the predicted data.
#' @param type A character to select a pre-defined set of metrics for "binary"
#'   and "regression" tasks. If not specified (default: `NULL`), the metrics
#'   that were specified during fitting the `object` are used.
#' @param ...  A list. Further arguments required to compute the performance
#'   metrics.
#'
#' @details
#' The performance metric has to be specified in the `object` that is used to
#'   carry out the experiment, i.e., [mlexperiments::MLCrossValidation] or
#'   [mlexperiments::MLNestedCV].
#'   Please note that the option `return_models = TRUE` must be set in the
#'   experiment class in order to be able to compute the predictions, which are
#'   required to conduct the calculation of the performance.
#'
#' @return The function returns a data.table with the computed performance
#'   metric of each fold.
#'
#' @examples
#' dataset <- do.call(
#'   cbind,
#'   c(sapply(paste0("col", 1:6), function(x) {
#'     rnorm(n = 500)
#'     },
#'     USE.NAMES = TRUE,
#'     simplify = FALSE
#'    ),
#'    list(target = sample(0:1, 500, TRUE))
#' ))
#'
#' fold_list <- splitTools::create_folds(
#'   y = dataset[, 7],
#'   k = 3,
#'   type = "stratified",
#'   seed = 123
#' )
#'
#' glm_optimization <- mlexperiments::MLCrossValidation$new(
#'   learner = LearnerGlm$new(),
#'   fold_list = fold_list,
#'   seed = 123
#' )
#'
#' glm_optimization$learner_args <- list(family = binomial(link = "logit"))
#' glm_optimization$predict_args <- list(type = "response")
#' glm_optimization$performance_metric_args <- list(
#'   positive = "1",
#'   negative = "0"
#' )
#' glm_optimization$performance_metric <- list(
#'   auc = metric("AUC"), sensitivity = metric("TPR"),
#'   specificity = metric("TNR")
#' )
#' glm_optimization$return_models <- TRUE
#'
#' # set data
#' glm_optimization$set_data(
#'   x = data.matrix(dataset[, -7]),
#'   y = dataset[, 7]
#' )
#'
#' cv_results <- glm_optimization$execute()
#'
#' # predictions
#' preds <- mlexperiments::predictions(
#'   object = glm_optimization,
#'   newdata = data.matrix(dataset[, -7]),
#'   na.rm = FALSE,
#'   ncores = 2L,
#'   type = "response"
#' )
#'
#' # performance
#' mlexperiments::performance(
#'   object = glm_optimization,
#'   prediction_results = preds,
#'   y_ground_truth = dataset[, 7],
#'   positive = "1"
#' )
#'
#' # performance - binary
#' mlexperiments::performance(
#'   object = glm_optimization,
#'   prediction_results = preds,
#'   y_ground_truth = dataset[, 7],
#'   type = "binary",
#'   positive = "1"
#' )
#'
#' @export
#'
performance <- function(
    object,
    prediction_results,
    y_ground_truth,
    type = NULL,
    ...
  ) {
  stopifnot(
    "`object` must be of class `MLCrossValidation`" =
      inherits(object, what = "MLCrossValidation"),
    "`object` must be an R6 class" = R6::is.R6(object),
    "`prediction_results` must be of class `mlexPredictions`" =
      inherits(prediction_results, "mlexPredictions")
  )

  kwargs <- list(...)
  model_names <- setdiff(colnames(prediction_results), c("mean", "sd"))
  perf_fun <- object$performance_metric

  perf_metric_args <- object$performance_metric_args

  if (!is.null(type)) {
    type <- match.arg(type, c("regression", "binary"))
    if (!requireNamespace("measures", quietly = TRUE)) {
      stop(
        paste0(
          "Package \"measures\" must be installed to use ",
          "function 'performance()'."
        ),
        call. = FALSE
      )
    }
    if (type == "regression") {
      append_metrics <- c(
        "MSE", "MAE", "MAPE", "RMSE", "RMSLE", "SSE", "RSQ"
      )
    } else if (type == "binary") {
      append_metrics <- .binary_metrics()
    }
    base_metric_list <- .metric_from_char(append_metrics)
    perf_fun <- kdry::list.append(perf_fun, base_metric_list)
  }

  res <- data.table::rbindlist(
    l = lapply(
      X = model_names,
      FUN = function(mn) {
        perf_args <- kdry::list.append(
          list(
            ground_truth = y_ground_truth,
            predictions = prediction_results[[mn]]
          ),
          kwargs
        )

        perf_args <- kdry::list.append(
          perf_args,
          perf_metric_args
        )

        dt_list <- kdry::list.append(
          list("model" = mn),
          .compute_performance(
            function_list = perf_fun,
            y = y_ground_truth,
            perf_args = perf_args
          )
        )
        data.table::setDT(dt_list)
        return(dt_list)
      }
    )
  )

  return(res)
}

.compute_performance <- function(
    function_list,
    y,
    perf_args
) {
  res <- sapply(
    X = names(function_list),
    FUN = function(x) {
      metric_types_helper(
        FUN = function_list[[x]],
        y = y,
        perf_args = perf_args
      )
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  return(res)
}

.binary_metrics <- function() {
  return(c(
    "AUC", "F1", "TPR", "TNR", "PPV", "NPV",
    "FNR", "FPR", "ACC", "BER", "BAC", "Brier", "BrierScaled"
  ))
}

.binary_metrics_probs <- function() {
  return(c(
    "AUC", "Brier", "BrierScaled"
  ))
}
