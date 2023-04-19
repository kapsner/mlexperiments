#' @title LearnerRpart R6 class
#'
#' @description
#' This learner is a wrapper around [rpart::rpart()] in order to fit recursive
#'   partitioning and regression trees.
#'
#'
#' @details
#' Optimization metric:
#' * classification (`method = "class"`): classification error rate
#' * regression (`method = "anova"`): mean squared error
#' *
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
#' Parameters that are specified with `parameter_grid` and / or `learner_args`
#'   are forwarded to `rpart`'s argument `control` (see
#'   [rpart::rpart.control()] for further details).
#'
#' For the two hyperparamter optimization strategies ("grid" and "bayesian"),
#'   the parameter `metric_optimization_higher_better` of the learner is
#'   set to `FALSE` by default as the classification error rate
#'   ([mlr3measures::ce()]) is used as the optimization metric for
#'   classification tasks and the mean squared error ([mlr3measures::mse()]) is
#'   used for regression tasks.
#'
#' @seealso [rpart::rpart()], [mlr3measures::ce()], [mlr3measures::mse()],
#'   [rpart::rpart.control()]
#'
#' @examples
#' LearnerRpart$new()
#'
#' @export
#'
LearnerRpart <- R6::R6Class( # nolint
  classname = "LearnerRpart",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    #'
    #' @description
    #' Create a new `LearnerRpart` object.
    #'
    #' @details
    #' This learner is a wrapper around [rpart::rpart()] in order to fit
    #'   recursive partitioning and regression trees. The following experiments
    #'   are implemented:
    #' * [mlexperiments::MLTuneParameters]
    #' * [mlexperiments::MLCrossValidation]
    #' * [mlexperiments::MLNestedCV]
    #'
    #' For the two hyperparamter optimization strategies ("grid" and
    #'   "bayesian"), the parameter `metric_optimization_higher_better` of the
    #'   learner is set to `FALSE` by default as the classification error rate
    #'   ([mlr3measures::ce()]) is used as the optimization metric for
    #'   classification tasks and the mean squared error ([mlr3measures::mse()])
    #'   is used for regression tasks.
    #'
    #' @seealso [rpart::rpart()], [mlr3measures::ce()], [mlr3measures::mse()]
    #'
    #' @examples
    #' LearnerRpart$new()
    #'
    #' @export
    #'
    initialize = function() {
      if (!requireNamespace("rpart", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"rpart\" must be installed to use ",
            "'learner = \"LearnerRpart\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(
        metric_optimization_higher_better = FALSE # classification error
      )
      self$environment <- "mlexperiments"
      self$cluster_export <- rpart_ce()
      private$fun_optim_cv <- rpart_optimization
      private$fun_fit <- function(x, y, ncores, seed, ...) {
        kwargs <- list(...)
        stopifnot(
          "`method` must be one of c('class', 'anova')" =
            kwargs$method %in% c("class", "anova")
        )
        args <- kdry::list.append(
          list(
            x = x, y = y, ncores = ncores, seed = seed
          ),
          kwargs
        )
        return(do.call(rpart_fit, args))
      }
      private$fun_predict <- rpart_predict
      private$fun_bayesian_scoring_function <- rpart_bsF
    }
  )
)


rpart_ce <- function() {
  c("rpart_optimization", "rpart_cv", "rpart_fit", "rpart_fit_fun",
    "rpart_predict_base", "rpart_predict", "metric")
}

rpart_bsF <- function(...) { # nolint
  params <- list(...)

  params <- kdry::list.append(
    main_list = params,
    append_list = method_helper$execute_params["cat_vars"]
  )

  # call to rpart_optimization here with ncores = 1, since the Bayesian search
  # is parallelized already / "FUN is fitted n times in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_rpart <- rpart_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_rpart$metric_optim_mean),
    bayes_opt_rpart
  )

  return(ret)
}

rpart_cv <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {

  outlist <- list()

  # loop over the folds
  for (fold in names(fold_list)) {

    # get row-ids of the current fold
    train_idx <- fold_list[[fold]]

    y_train <- kdry::mlh_subset(y, train_idx)

    # train the model for this cv-fold
    args <- kdry::list.append(
      list(
        y = y_train,
        x = kdry::mlh_subset(x, train_idx),
        ncores = ncores,
        seed = seed
      ),
      params
    )
    set.seed(seed)
    cvfit <- do.call(rpart_fit, args)
    outlist[[fold]] <- list(cvfit = cvfit,
                            train_idx = train_idx)
  }
  return(outlist)
}

rpart_optimization <- function(x, y, params, fold_list, ncores, seed) {
  stopifnot(
    "`params` must be a list" = is.list(params),
    "One item of `params` must be `method`" = "method" %in% names(params),
    "`method` must be one of c('class', 'anova')" =
      params$method %in% c("class", "anova")
  )

  # check, if this is a classification context and select metric accordingly
  if (params$method == "class") {
    msg <- "Classification: using 'classification error rate'"
    FUN <- mlexperiments::metric("ce") # nolint
    pred_type <- "class"
  } else {
    msg <- "Regression: using 'mean squared error'"
    FUN <- mlexperiments::metric("mse") # nolint
    pred_type <- "vector"
  }
  message(paste("\n", msg, "as optimization metric."))

  args <- list(
    x = x,
    y = y,
    params = params,
    fold_list = fold_list,
    ncores = ncores,
    seed = seed
  )
  cv_fit_list <- do.call(rpart_cv, args)

  # initialize a dataframe to store the results
  results_df <- data.table::data.table(
    "fold" = character(0),
    "metric" = numeric(0)
  )

  for (fold in names(cv_fit_list)) {

    cvfit <- cv_fit_list[[fold]][["cvfit"]]
    train_idx <- cv_fit_list[[fold]][["train_idx"]]

    pred_args <- list(
      model = cvfit,
      newdata = kdry::mlh_subset(x, -train_idx),
      ncores = ncores,
      type = pred_type,
      cat_vars = params$cat_vars
    )

    preds <- do.call(rpart_predict, pred_args)

    perf_args <- list(
      predictions = preds,
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

rpart_fit_fun <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  var_handler <- handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  rpart_params <- var_handler$params

  rpart_formula <- stats::as.formula(object = "rpart_y_train ~ .")

  rpart_y_train <- y # nolint
  train_x <- x

  rpart_control <- NULL
  rpart_control_default <- formals(rpart::rpart.control)
  for (update_arg in names(rpart_params)) {
    control_list <- list()
    if (update_arg %in% names(rpart_control_default)) {
      control_list <- c(
        control_list,
        rpart_params[update_arg])
      # delete item from rpart_params
      rpart_params[[update_arg]] <- NULL
    }
    if (length(control_list) > 0L) {
      rpart_control <- do.call(rpart::rpart.control, control_list)
    }
  }

  args <- kdry::list.append(
    list(
      formula = rpart_formula,
      data = kdry::dtr_matrix2df(train_x, cat_vars = cat_vars)
    ),
    rpart_params
  )
  args <- kdry::list.append(
    args,
    list(control = rpart_control)
  )
  set.seed(seed)
  fit <- do.call(rpart::rpart, args)
  return(fit)
}

rpart_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot("method" %in% names(kwargs))
  fit_args <- kdry::list.append(
    list(
      x = x,
      y = y,
      ncores = ncores,
      seed = seed
    ),
    kwargs
  )
  return(do.call(rpart_fit_fun, fit_args))
}

rpart_predict_base <- function(model, newdata, ncores, kwargs) {
  var_handler <- handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  rpart_params <- var_handler$params

  predict_args <- kdry::list.append(
    list(
      object = model,
      newdata = kdry::dtr_matrix2df(matrix = newdata, cat_vars = cat_vars)
    ),
    rpart_params
  )

  return(do.call(stats::predict, predict_args))
}

rpart_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)

  args <- list(
    model = model,
    newdata = newdata,
    ncores = ncores,
    kwargs = kwargs
  )
  preds <- do.call(rpart_predict_base, args)

  if ("type" %in% names(kwargs)) {
    if (kwargs$type == "prob" && ncol(preds) == 2) { # in case of binary classif
      preds <- as.vector(preds[, 2])
    }
  }

  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <- kdry::mlh_reshape(preds)
    }
  }
  return(preds)
}
