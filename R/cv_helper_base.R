.run_cv <- function(self, private) {
  cv_results <- .fold_looper(self, private)
  outlist <- .cv_postprocessing(
    self = self,
    private = private,
    results_object = cv_results,
    metric_higher_better = private$metric_performance_higher_better
  )
  class(outlist) <- c("list", "mlexCV")
  self$results <- outlist
  return(self$results$summary)
}

.fold_looper <- function(self, private) {
  # init a progress bar
  pb <- progress::progress_bar$new(
    format = "CV progress [:bar] :current/:total (:percent)\n",
    total = length(self$fold_list)
  )

  outlist <- list()

  for (fold in names(self$fold_list)) {
    message(paste0("CV fold: ", fold))
    # increment progress bar
    pb$tick()

    # get fold ids
    train_index <- self$fold_list[[fold]]

    fold_train <- list(x = .format_xy(private$x, train_index),
                       y = .format_xy(private$y, train_index))
    fold_test <- list(x = .format_xy(private$x, -train_index),
                      y = .format_xy(private$y, -train_index))

    run_args <- list(
      train_index = train_index,
      fold_train = fold_train,
      fold_test = fold_test
    )

    # for nested cv, just overwrite private$cv_run_model in inherited
    # class and add hyperparameter search before calling .cv_run_model

    outlist[[fold]] <- do.call(private$cv_run_model, run_args)
  }
  return(outlist)
}


.cv_postprocessing <- function(
    self,
    private,
    results_object,
    metric_higher_better
) {

  # calculate error metric for each fold
  for (fold in names(results_object)) {
    results_object[[fold]][["performance"]] <-
      private$fun_performance_metric(
        ground_truth = results_object[[fold]][["ground_truth"]],
        predictions = results_object[[fold]][["predictions"]]
      )
  }

  # calculate performance metrics here
  # add them to a nice results table
  results_object[["summary"]] <- data.table::rbindlist(
    l = sapply(
      X = names(results_object),
      FUN = function(x) {
        # which learner args should be added in the final output?
        add_args <- vapply(
          X = results_object[[x]][["learner.args"]],
          FUN = function(test_args) {
            ifelse(
              test = length(test_args) == 1L && is.atomic(test_args),
              yes = TRUE,
              no = FALSE
            )
          },
          FUN.VALUE = logical(1L)
        )

        outlist <- list("performance" = results_object[[x]][["performance"]])

        if (sum(add_args) > 0) {
          outlist <- c(outlist, results_object[[x]][["learner.args"]][add_args])
        }
        return(outlist)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  )
  # return
  return(results_object)
}

.cv_fit_model <- function(self, private, train_index, fold_train, fold_test) {

  fit_args <- list(
      x = fold_train$x,
      y = fold_train$y,
      seed = private$seed,
      ncores = private$ncores
    )

  if (is.list(self$learner_args)) {

    learner_args <- self$learner_args
    learner_args <- .method_params_refactor(learner_args, private$method_helper)
    learner_args <- learner_args[!duplicated_by_names(
      learner_args, fromLast = TRUE
    )]

    fit_args <- c(
      fit_args,
      learner_args
    )
  } else {
    learner_args <- NULL
  }

  fit_args <- .eval_params(fit_args)

  # initialize learner here for every model fit
  learner <- self$learner$new()
  if (is.null(private$metric_performance_higher_better)) {
    private$metric_performance_higher_better <-
      learner$metric_performance_higher_better
  }

  if (is.null(private$fun_performance_metric)) {
    private$fun_performance_metric <-
      learner$performance_metric
  }
  set.seed(private$seed)
  fitted <- do.call(learner$fit, fit_args)

  # make predictions
  pred_args <- list(
    model = fitted,
    newdata = fold_test$x,
    ncores = private$ncores
  )
  if (!is.null(private$cat_vars)) {
    pred_args <- c(pred_args, list(cat_vars = private$cat_vars))
  }
  preds <- do.call(learner$predict, pred_args)

  res <- list(
    fold_ids = train_index,
    ground_truth = fold_test$y,
    predictions = preds,
    "learner.args" = learner_args
  )

  if (self$return_models) {
    res <- c(res, list(model = fitted))
  }
  return(res)
}
