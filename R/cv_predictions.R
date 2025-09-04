#' @title predictions
#'
#' @description Apply an R6 object of class `"MLCrossValidation"` to new data
#'   to compute predictions.
#'
#' @param object An R6 object of class `"MLCrossValidation"` for which the
#'   predictions should be computed.
#' @param newdata The new data for which predictions should be made using
#'   the `model`.
#' @param na.rm A logical. If missings should be removed before computing the
#'   mean and standard deviation of the performance across different folds for
#'   each observation in `newdata`.
#' @param ncores An integer to specify the number of cores used for
#'   parallelization (default: `-1L`).
#' @param ...  A list. Further arguments required to compute the predictions.
#'
#' @return The function returns a data.table of class `"mlexPredictions"`with
#'   one row for each observation in `newdata` and the columns containing
#'   the predictions for each fold, along with the mean and standard deviation
#'   across all folds.
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
#'    positive = 1,
#'    negative =0
#' )
#' glm_optimization$performance_metric <- metric("AUC")
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
#' head(preds)
#'
#' @export
#'
predictions <- function(
    object,
    newdata,
    na.rm = FALSE, # nolint
    ncores = -1L,
    ...
  ) {
  stopifnot(
    "`ncores` must be an integer" = is.integer(as.integer(ncores)),
    "`ncores` must not be `0L`" = ncores != 0L,
    "`object` must be of class `MLCrossValidation`" =
      inherits(object, what = "MLCrossValidation"),
    "`object` must be an R6-class" = R6::is.R6(object),
    "`object$results` must be of class `mlexCV`" =
      inherits(object$results, "mlexCV"),
    "`object$return_models` must be `TRUE`" = isTRUE(object$return_models),
    "`na.rm` must be a boolean value" = is.logical(na.rm)
  )
  kwargs <- list(...)
  ncores <- kdry::pch_check_available_cores(ncores = as.integer(ncores))
  model_names <- names(object$results$folds)
  pred_fun <- object$learner$predict

  # add base-vars + categorical variables
  pred_args_base <- kdry::list.append(
    main_list = list(
      newdata = newdata,
      ncores = ncores
    ),
    append_list =
      object$.__enclos_env__$private$method_helper$execute_params["cat_vars"]
  )
  # add kwargs
  pred_args_base <- kdry::list.append(
    pred_args_base,
    kwargs
  )
  # add predict args from object
  pred_args_base <- kdry::list.append(
    pred_args_base,
    object$predict_args
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

  res <- data.table::as.data.table(res_pre)

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
