.cv_run_nested_model <- function(
    self,
    private,
    train_index,
    fold_train,
    fold_test
) {

  hparam_tuner <- MLTuneParameters$new(
    learner = self$learner,
    strategy = self$strategy,
    seed = private$seed,
    ncores = private$ncores
  )
  hparam_tuner$parameter_bounds <- self$parameter_bounds
  hparam_tuner$parameter_grid <-
    private$method_helper$execute_params$parameter_grid
  hparam_tuner$learner_args <-
    private$method_helper$execute_params$params_not_optimized

  if ("case_weights" %in% names(hparam_tuner$learner_args)) {
    hparam_tuner$learner_args$case_weights <- kdry::mlh_subset(
      private$method_helper$execute_params$params_not_optimized$case_weights,
      train_index
    )
  }

  hparam_tuner$optim_args <- self$optim_args
  hparam_tuner$split_type <- self$split_type
  hparam_tuner$split_vector <- self$split_vector[train_index]
  # run hyper parameter optimization on training fold
  hparam_tuner$set_data(
    x = fold_train$x,
    y = fold_train$y,
    cat_vars = private$method_helper$execute_params$cat_vars
  )

  # execute optimization
  hparam_tuner$execute(k = self$k_tuning)

  outlist <- list(results.optimization = hparam_tuner$results)

  # adjust best settings to fit final modle with
  learner_args <- hparam_tuner$results[["best.setting"]]
  learner_args <- learner_args[(names(learner_args) != "setting_id")]
  learner_args <- learner_args[!kdry::misc_duplicated_by_names(
    learner_args, fromLast = TRUE
  )]

  if ("case_weights" %in% names(learner_args)) {
    learner_args$case_weights <- NULL
  }

  self$learner_args <- learner_args

  # fit final model
  res <- .cv_fit_model(
    self = self,
    private = private,
    train_index = train_index,
    fold_train = fold_train,
    fold_test = fold_test
  )
  outlist <- kdry::list.append(outlist, res)
  return(outlist)
}
