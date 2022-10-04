#' @export
LearnerSurvXgboostCox <- R6::R6Class( # nolint
  classname = "LearnerSurvXgboostCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"xgboost\" must be installed to use ",
            "'learner = \"LearnerSurvXgboostCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_optimization_higher_better <- FALSE
      self$metric_performance_higher_better <- TRUE
      self$environment <- "mlexperiments"
      self$cluster_export <- surv_xgboost_cox_ce()
      private$fun_optim_cv <- surv_xgboost_cox_optimization
      private$fun_fit <- surv_xgboost_cox_fit
      private$fun_predict <- surv_xgboost_cox_predict
      private$fun_bayesian_scoring_function <- surv_xgboost_cox_bsF
      private$fun_performance_metric <- surv_xgboost_c_index
      self$metric_performance_name <- "C-index"
    }
  )
)


surv_xgboost_cox_ce <- function() {
  c("surv_xgboost_cox_optimization", "surv_xgboost_cox_fit",
    "setup_surv_xgb_dataset", ".outsample_row_indices")
}

surv_xgboost_cox_bsF <- function(...) { # nolint

  params <- list(...)
  params <- .method_params_refactor(params, method_helper)

  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_xgboost <- surv_xgboost_cox_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- c(
    list("Score" = bayes_opt_xgboost$metric_optim_mean),
    bayes_opt_xgboost
  )

  return(ret)
}

# tune lambda
surv_xgboost_cox_optimization <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {
  stopifnot(
    inherits(x = y, what = "Surv"),
    is.list(params),
    params$objective == "survival:cox"
  )

  dtrain <- setup_surv_xgb_dataset(
    x = x,
    y = y,
    objective = params$objective
  )

  # use the same folds for all algorithms
  # folds: list provides a possibility to use a list of pre-defined CV
  # folds (each element must be a vector of test fold's indices).
  # When folds are supplied, the nfold and stratified parameters
  # are ignored.
  xgb_fids <- .outsample_row_indices(
    fold_list = fold_list,
    training_data = nrow(x)
  )

  set.seed(seed)
  # train the model for this cv-fold
  cvfit <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = as.integer(options("mlexperiments.optim.xgb.nrounds")),
    prediction = FALSE,
    folds = xgb_fids,
    verbose = FALSE,
    print_every_n = as.integer(options("mlexperiments.xgb.print_every_n")),
    early_stopping_rounds = as.integer(
      options("mlexperiments.optim.xgb.early_stopping_rounds")
    ),
    nthread = ncores
  )

  # save the results / use xgboost's metric here for selecting the best model
  # (cox-nloglik)
  metric_col <- grep(
    pattern = "^test(.*)mean$",
    x = colnames(cvfit$evaluation_log),
    value = TRUE
  )
  stopifnot(length(metric_col) == 1)

  res <- list(
    "metric_optim_mean" = cvfit$evaluation_log[
      get("iter") == cvfit$best_iteration,
      get(metric_col)
    ],
    "nrounds" = cvfit$best_iteration
  )

  return(res)
}

surv_xgboost_cox_fit <- function(x, y, nrounds, ncores, seed, ...) {
  params <- list(...)
  stopifnot("objective" %in% names(params))
  # train final model with best nrounds
  dtrain_full <- setup_surv_xgb_dataset(
    x = x,
    y = y,
    objective = params$objective
  )

  # setup a watchlist (the training data here)
  watchlist <- list(train = dtrain_full)

  set.seed(seed)
  # fit the model
  bst <- xgboost::xgb.train(
    data = dtrain_full,
    params = params,
    print_every_n = as.integer(options("mlexperiments.xgb.print_every_n")),
    nthread = ncores,
    nrounds = nrounds,
    watchlist = watchlist,
    maximize = NULL,
    verbose = FALSE
  )
  return(bst)
}

# wrapper function for creating the input data for xgboost
setup_surv_xgb_dataset <- function(x, y, objective) {

  # create a xgb.DMatrix
  dtrain <- xgboost::xgb.DMatrix(x)

  # for aft-models, the label must be formatted as follows:
  if (objective == "survival:aft") {
    y_lower_bound <- y[, 1]
    y_upper_bound <- ifelse(
      y[, 2] == 1,
      y[, 1],
      Inf
    )
    xgboost::setinfo(dtrain, "label_lower_bound", y_lower_bound)
    xgboost::setinfo(dtrain, "label_upper_bound", y_upper_bound)
  } else if (objective == "survival:cox") {
    # Cox regression for right censored survival time data (negative values
    # are considered right censored). Note that predictions are returned on
    # the hazard ratio scale (i.e., as HR = exp(marginal_prediction) in
    # the proportional hazard function h(t) = h0(t) * HR).
    label <- ifelse(y[, 2] == 1, y[, 1], -y[, 1])
    xgboost::setinfo(dtrain, "label", label)
  } else {
    stop(paste0("xgboost objective '", objective, " 'not implemented."))
  }

  return(dtrain)
}

surv_xgboost_cox_predict <- function(model, newdata, ncores, ...) {
  # From the docs:
  # Note that predictions are returned on the hazard ratio scale
  # (i.e., as HR = exp(marginal_prediction) in the proportional
  # hazard function h(t) = h0(t) * HR).
  return(predict(object = model, newdata = newdata, ...))
}

surv_xgboost_c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
