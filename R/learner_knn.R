#' LearnerKnn R6 class
#'
#' @description
#' This learner is a wrapper around [class::knn()] in order to perform a
#'   k-nearest neighbour classification. The following experiments are
#'   implemented:
#'   \enumerate{
#'   \item `mlexperiments::MLTuneParameters`
#'   \item `mlexperiments::MLCrossValidation`
#'   \item `mlexperiments::MLNestedCV`
#'   }
#'
#'
#' @details
#' Implemented methods:
#' \describe{
#' \item{`$fit`}{To fit the model.}
#' \item{`$predict`}{To predict new data with the model.}
#' \item{`$cross_validation`}{To perform a grid search (hyperparameter
#'   optimization).}
#' \item{`$bayesian_scoring_function`}{To perform a Bayesian hyperparameter
#'   optimization.}
#' }
#'
#' For the two hyperparamter optimization strategies ("grid" and "bayesian"),
#'   the parameter `metric_optimization_higher_better` of the learner is
#'   set to `FALSE` by default as the classification error rate
#'   ([mlr3measures::ce()]) is used as the optimization metric.
#'
#' @seealso [class::knn()], [mlr3measures::ce()]
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
    #'   k-nearest neighbour classification. The following experiments are
    #'   implemented:
    #'   \enumerate{
    #'   \item `mlexperiments::MLTuneParameters`
    #'   \item `mlexperiments::MLCrossValidation`
    #'   \item `mlexperiments::MLNestedCV`
    #'   }
    #' For the two hyperparamter optimization strategies ("grid" and
    #'   "bayesian"), the parameter `metric_optimization_higher_better` of the
    #'   learner is set to `FALSE` by default as the classification error rate
    #'   ([mlr3measures::ce()]) is used as the optimization metric.
    #'
    #' @seealso [class::knn()], [mlr3measures::ce()]
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
  c("knn_optimization", "knn_fit", "knn_predict", "metric", ".format_xy")
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

# tune lambda
knn_optimization <- function(x, y, params, fold_list, ncores, seed) {
  stopifnot(
    is.list(params),
    "k" %in% names(params)
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
        x = .format_xy(x, train_idx),
        test = .format_xy(x, -train_idx),
        y = .format_xy(y, train_idx),
        use.all = FALSE,
        ncores = ncores,
        seed = seed
      ),
      params
    )
    set.seed(seed)
    cvfit <- do.call(knn_fit, args)

    # optimize error rate
    FUN <- metric("ce") # nolint
    err <- FUN(
      predictions = knn_predict(
        model = cvfit,
        newdata = .format_xy(x, -train_idx),
        ncores = ncores,
        type = "response"
      ),
      ground_truth = .format_xy(y, -train_idx)
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
          "validation_metric" = err
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
  stopifnot("k" %in% names(kwargs))

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
  stopifnot("type" %in% names(kwargs))

  if (kwargs$type == "response") {
    return(model)
  } else if (kwargs$type == "prob") {
    # there is no knn-model but the probabilities predicted for the test data
    return(attributes(model)$prob)
  }
}
