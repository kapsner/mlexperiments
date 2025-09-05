#' @title LearnerKnn R6 class
#'
#' @description
#' This learner is a wrapper around [class::knn()] in order to perform a
#'   k-nearest neighbor classification.
#'
#'
#' @details
#' Optimization metric: classification error rate
#' Can be used with
#' * [mlexperiments::MLTuneParameters]
#' * [mlexperiments::MLCrossValidation]
#' * [mlexperiments::MLNestedCV]
#'
#' Implemented methods:
#' * `$fit` To fit the model.
#' * `$predict` To predict new data with the model.
#' * `$cross_validation` To perform a grid search (hyperparameter
#'   optimization).
#' * `$bayesian_scoring_function` To perform a Bayesian hyperparameter
#'   optimization.
#'
#' For the two hyperparameter optimization strategies ("grid" and "bayesian"),
#'   the parameter `metric_optimization_higher_better` of the learner is
#'   set to `FALSE` by default as the mean misclassification error
#'   ([measures::MMCE()]) is used as the optimization metric.
#'
#' @seealso [class::knn()], [measures::MMCE()]
#'
#' @examples
#' LearnerKnn$new()
#'
#' @export
#'
LearnerKnn <- R6::R6Class( # nolint
  classname = "LearnerKnn",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    #'
    #' @description
    #' Create a new `LearnerKnn` object.
    #'
    #' @details
    #' This learner is a wrapper around [class::knn()] in order to perform a
    #'   k-nearest neighbor classification. The following experiments are
    #'   implemented:
    #' * [mlexperiments::MLTuneParameters]
    #' * [mlexperiments::MLCrossValidation]
    #' * [mlexperiments::MLNestedCV]
    #' For the two hyperparameter optimization strategies ("grid" and
    #'   "bayesian"), the parameter `metric_optimization_higher_better` of the
    #'   learner is set to `FALSE` by default as the mean misclassification
    #'   error ([measures::MMCE()]) is used as the optimization metric.
    #'
    #' @seealso [class::knn()], [measures::MMCE()]
    #'
    #' @examples
    #' LearnerKnn$new()
    #'
    #' @export
    #'
    initialize = function() {
      if (!requireNamespace("class", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"class\" must be installed to use ",
            "'learner = \"LearnerKnn\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(
        metric_optimization_higher_better = FALSE # classification error
      )
      self$environment <- "mlexperiments"
      self$cluster_export <- knn_ce()
      private$fun_optim_cv <- knn_optimization
      private$fun_fit <- knn_fit
      private$fun_predict <- knn_predict
      private$fun_bayesian_scoring_function <- knn_bsF
    }
  )
)


knn_ce <- function() {
  c("knn_optimization", "knn_fit", "knn_predict", "metric")
}

knn_bsF <- function(...) { # nolint
  params <- list(...)

  # call to knn_optimization here with ncores = 1, since the Bayesian search
  # is parallelized already / "FUN is fitted n times in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_knn <- knn_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_knn$metric_optim_mean),
    bayes_opt_knn
  )

  return(ret)
}

knn_optimization <- function(x, y, params, fold_list, ncores, seed) {
  stopifnot(
    "`params` must be a list" = is.list(params),
    "One item of `params` must be `k`" = "k" %in% names(params)
  )

  # initialize a dataframe to store the results
  results_df <- data.table::data.table(
    "fold" = character(0),
    "metric" = numeric(0)
  )

  # we do not need test here as it is defined explicitly below
  params[["test"]] <- NULL

  # loop over the folds
  for (fold in names(fold_list)) {

    # get row-ids of the current fold
    train_idx <- fold_list[[fold]]

    # train the model for this cv-fold
    args <- kdry::list.append(
      list(
        x = kdry::mlh_subset(x, train_idx),
        test = kdry::mlh_subset(x, -train_idx),
        y = kdry::mlh_subset(y, train_idx),
        use.all = FALSE,
        ncores = ncores,
        seed = seed
      ),
      params
    )
    set.seed(seed)
    cvfit <- do.call(knn_fit, args)

    # optimize error rate
    FUN <- metric("MMCE") # nolint
    perf_args <- list(
      predictions = knn_predict(
        model = cvfit,
        newdata = kdry::mlh_subset(x, -train_idx),
        ncores = ncores,
        type = "response"
      ),
      ground_truth = kdry::mlh_subset(y, -train_idx)
    )
    perf <- metric_types_helper(
      FUN = FUN,
      y = y,
      perf_args = perf_args
    )

    results_df <- data.table::rbindlist(
      l = list(
        results_df,
        list(
          "fold" = fold,
          "validation_metric" = perf
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

knn_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot("`k` is a required argument" = "k" %in% names(kwargs))

  args <- kdry::list.append(
    list(
      train = x,
      cl = y
    ),
    kwargs
  )
  args$prob <- TRUE
  set.seed(seed)
  fit <- do.call(class::knn, args)
  return(fit)
}

knn_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)
  stopifnot("`type` is a required argument" = "type" %in% names(kwargs))

  if (kwargs$type == "response") {
    preds <- model
  } else if (kwargs$type == "prob") {
    # there is no knn-model but the probabilities predicted for the test data
    preds <- attributes(model)$prob
    preds <- .expand_predictions(preds = preds, classes = model)
  }
  return(preds)
}

.expand_preds_helper <- function(pred_row, name, prob) {
  pred_row[name] <- prob
  return(pred_row)
}

.expand_predictions <- function(preds, classes) {

  names(preds) <- classes

  c_names <- unique(names(preds))
  pred_row <- rep(0, length(c_names))
  names(pred_row) <- c_names

  pred_list <- lapply(
    X = seq_along(preds),
    FUN = function(x) {
      .expand_preds_helper(
        pred_row = pred_row,
        name = names(preds[x]),
        prob = preds[x]
      )
    }
  )
  return(do.call(rbind, pred_list))
}
