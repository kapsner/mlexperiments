.run_cv <- function(self, private) {
  cv_results <- .fold_looper(self, private)
  outlist <- .cv_postprocessing(
    self = self,
    private = private,
    results_object = cv_results
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
    message(paste0("\nCV fold: ", fold))
    # increment progress bar
    pb$tick()

    # get fold ids
    train_index <- self$fold_list[[fold]]

    fold_train <- list(x = kdry::mlh_subset(private$x, train_index),
                       y = kdry::mlh_subset(private$y, train_index))
    fold_test <- list(x = kdry::mlh_subset(private$x, -train_index),
                      y = kdry::mlh_subset(private$y, -train_index))

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
    results_object
) {

  outlist <- list(folds = results_object)

  # calculate error metric for each fold
  for (fold in names(results_object)) {
    perf_args <- kdry::list.append(
      list(
        ground_truth = results_object[[fold]][["ground_truth"]],
        predictions = results_object[[fold]][["predictions"]]
      ),
      self$performance_metric_args
    )
    outlist[["performance"]][[fold]] <- .compute_performance(
      function_list = self$performance_metric,
      y = private$y,
      perf_args = perf_args
    )
  }

  # calculate performance metrics here
  # add them to a nice results table
  outlist[["summary"]] <- data.table::rbindlist(
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


        if ("cat_vars" %in% names(add_args)) {
          add_args["cat_vars"] <- FALSE
        }

        ret <- c(
          list("fold" = x),
          outlist[["performance"]][[x]]
        )

        if (sum(add_args) > 0) {
          ret <- kdry::list.append(
            ret,
            results_object[[x]][["learner.args"]][add_args]
          )
        }
        return(ret)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  )
  # return
  return(outlist)
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
    learner_args <- .eval_params(learner_args)

    if ("case_weights" %in% names(learner_args)) {
      learner_args$case_weights <- kdry::mlh_subset(
        private$method_helper$execute_params$params_not_optimized$case_weights, # nolint
        train_index
      )
    }

  } else {
    learner_args <- NULL
  }

  fit_args <- kdry::list.append(
    fit_args,
    learner_args
  )

  fit_args <- kdry::list.append(
    main_list = fit_args,
    append_list = list(cat_vars = learner_args$cat_vars)
  )

  set.seed(private$seed)
  fitted <- do.call(self$learner$fit, fit_args)

  # make predictions
  pred_args <- kdry::list.append(
    main_list = list(
      model = fitted,
      newdata = fold_test$x,
      ncores = private$ncores
    ),
    append_list = list(cat_vars = learner_args$cat_vars)
  )
  pred_args <- kdry::list.append(pred_args, self$predict_args)

  preds <- do.call(self$learner$predict, pred_args)

  res <- list(
    fold_ids = train_index,
    ground_truth = fold_test$y,
    predictions = preds,
    "learner.args" = learner_args
  )

  if (self$return_models) {
    res <- kdry::list.append(res, list(model = fitted))
  }
  return(res)
}
