#' @export
performance <- function(object, prediction_results, y_test, ...) {
  stopifnot(
    inherits(object, what = "MLCrossValidation"),
    R6::is.R6(object),
    inherits(prediction_results, "mlexPredictions")
  )
  kwargs <- list(...)
  model_names <- setdiff(colnames(prediction_results), c("mean", "sd"))
  perf_fun <- object$performance_metric

  res <- data.table::rbindlist(
    l = lapply(
      X = model_names,
      FUN = function(mn) {
        perf_args <- kdry::list.append(
          list(
            ground_truth = y_test,
            predictions = prediction_results[[mn]]
          ),
          kwargs
        )

        data.table::data.table(
          "model" = mn,
          "performance" = do.call(perf_fun, perf_args)
        )
      }
    )
  )
  return(res)
}
