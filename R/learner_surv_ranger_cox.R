#' @export
LearnerSurvRangerCox <- R6::R6Class( # nolint
  classname = "LearnerSurvRangerCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"ranger\" must be installed to use ",
            "'learner = \"LearnerSurvRangerCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_optimization_higher_better <- FALSE
      self$metric_performance_higher_better <- TRUE
      self$environment <- "mlexperiments"
      self$cluster_export <- surv_ranger_cox_ce()
      private$fun_optim_cv <- surv_ranger_cox_optimization
      private$fun_fit <- surv_ranger_cox_fit
      private$fun_predict <- surv_ranger_cox_predict
      private$fun_bayesian_scoring_function <- surv_ranger_cox_bsF
      private$fun_performance_metric <- surv_ranger_c_index
      self$metric_performance_name <- "C-index"
    }
  )
)


surv_ranger_cox_ce <- function() {
  c("surv_ranger_cox_optimization", "surv_ranger_cox_fit", "surv_ranger_cox_predict",
    ".outsample_row_indices", ".matrix_to_df", "surv_ranger_c_index")
}

surv_ranger_cox_bsF <- function(...) { # nolint

  params <- list(...)

  params <- .method_params_refactor(params, method_helper)

  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_ranger <- surv_ranger_cox_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- c(
    list("Score" = bayes_opt_ranger$metric_optim_mean),
    bayes_opt_ranger
  )

  return(ret)
}

# tune lambda
surv_ranger_cox_optimization <- function(x, y, params, fold_list, ncores, seed) {
  stopifnot(
    inherits(x = y, what = "Surv"),
    is.list(params)
  )

  # initialize a dataframe to store the results
  results_df <- data.table::data.table(
    "fold" = character(0),
    "metric" = numeric(0)
  )

  # currently, there is no cross validation implemented in the ranger package.
  # as the code has already been written for xgboost, I just adapt it here
  # to work for survival models with ranger and to accept a list of parameters
  # from the parmeter grid-search.

  # loop over the folds
  for (fold in names(fold_list)) {

    # get row-ids of the current fold
    ranger_train_idx <- fold_list[[fold]]

    # train the model for this cv-fold
    args <- c(list(
      x = x[ranger_train_idx, ],
      y = y[ranger_train_idx, ],
      ncores = ncores,
      seed = seed
    ),
    params
    )
    set.seed(seed)
    cvfit <- do.call(surv_ranger_cox_fit, args)

    # create predictions for calculating the c-index
    preds <- surv_ranger_cox_predict(
      model = cvfit,
      newdata = x[-ranger_train_idx, ],
      cat_vars = params[["cat_vars"]],
      num.threads = ncores
    )

    # calculate Harrell's c-index using the `glmnet::Cindex`-implementation
    c_index <- surv_ranger_c_index(
      predictions = preds,
      ground_truth = y[-ranger_train_idx, ]
    )


    # save the results of this fold into a dataframe
    # from help("ranger::ranger"):
    # prediction.error - Overall out of bag prediction error. [...] for
    # survival one minus Harrell's C-index.
    results_df <- data.table::rbindlist(
      l = list(
        results_df,
        list(
          "fold" = fold,
          "oob_metric" = 1 - cvfit$prediction.error,
          "validation_metric" = c_index
        )
      ),
      fill = TRUE
    )
  }

  res <- list(
    "metric_optim_mean" = mean(results_df$validation_metric)
  )

  return(res)
}

# pass parameters as ...
surv_ranger_cox_fit <- function(x, y, ncores, seed, ...) {
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
    ranger_params <- params[names(params) != "cat_vars"]
  } else {
    cat_vars <- NULL
    ranger_params <- params
  }

  x <- .matrix_to_df(matrix = x, cat_vars = cat_vars)

  args <- c(list(
    x = x,
    y = y,
    num.threads = ncores,
    oob.error = TRUE
  ),
  ranger_params
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(ranger::ranger, args)
  return(bst)
}

surv_ranger_cox_predict <- function(model, newdata, ...) {

  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
    ranger_params <- params[names(params) != "cat_vars"]
  } else {
    cat_vars <- NULL
    ranger_params <- params
  }

  newdata <- .matrix_to_df(matrix = newdata, cat_vars = cat_vars)

  # From the docs:
  # For type = 'response' (the default), the [...] survival probabilities
  # (survival) are returned.

  # ranger returns the survival probability S(t), which is the conditional
  # probability that a subject survives >= t, given that is has survived until t
  #
  preds <- predict(
    object = model,
    data = newdata,
    type = "response",
    ranger_params
  )
  # https://github.com/imbs-hl/ranger/issues/617#issuecomment-1144443486
  # Internally, ranger uses the sum of chf over time to calculate the c-index,
  # i.e. rowSums(preds_ranger_prep$chf)
  pred_probs <- rowSums(preds$chf)

  # The Integrated/Cumulative Harzard H(t) = -log(S(t))
  # time_point <- which(preds$unique.death.times == max(preds$unique.death.times))
  # pred_probs <- -log(preds$survival[, time_point])
  return(pred_probs)
}

surv_ranger_c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
