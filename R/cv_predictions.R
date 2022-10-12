#' @export
predictions <- function(
    object,
    newdata,
    na.rm = FALSE, # nolint
    ncores = -1L,
    ...
  ) {
  stopifnot(
    is.integer(as.integer(ncores)),
    ncores != 0L,
    inherits(object, what = "MLCrossValidation"),
    R6::is.R6(object),
    inherits(object$results, "mlexCV"),
    isTRUE(object$return_models),
    is.logical(na.rm)
  )
  kwargs <- list(...)
  ncores <- kdry::pch_check_available_cores(ncores = as.integer(ncores))
  model_names <- names(object$results$folds)
  pred_fun <- object$learner$predict

  pred_args_base <- kdry::list.append(
    list(
      newdata = newdata,
      ncores = ncores
    ),
    kwargs
  )

  res_pre <- sapply(
    X = model_names,
    FUN = function(m) {
      pred_args <- kdry::list.append(
        list(model = object$results$folds[[m]]$model),
        pred_args_base
      )

      do.call(pred_fun, pred_args)
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  res <- data.table::as.data.table(do.call(cbind, res_pre))

  res[, `:=`(
    mean = mean(as.numeric(.SD), na.rm = na.rm),
    sd = stats::sd(as.numeric(.SD), na.rm = na.rm)
    ),
    .SDcols = colnames(res),
    by = seq_len(nrow(res))
  ]
  class(res) <- c("data.frame", "data.table", "mlexPredictions")
  return(res)
}
