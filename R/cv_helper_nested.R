.cv_run_nested_model <- function(self, private, train_index, fold_train, fold_test) {

  hparam_tuner <- MLTuneParameters$new(
    learner = learner,
    strategy = self$strategy,
    seed = private$seed,
    ncores = private$ncores
  )
  hparam_tuner$parameter_bounds <- self$parameter_bounds
  hparam_tuner$parameter_grid <- self$parameter_grid
  hparam_tuner$optim_args <- self$optim_args
  hparam_tuner$split_type <- self$split_type
  hparam_tuner$split_vector <- self$split_vector[train_index]
  # run hyper parameter optimization on training fold
  hparam_tuner$set_data(x = fold_train$x, y = fold_train$y)

  # execute optimization
  hparam_tuner$execute(k = self$k_tuning)

  outlist <- list(results.optimization = hparam_tuner$results)

  # adjust best settings to fit final modle with
  learner_args <- hparam_tuner$results[["best.setting"]]
  self$learner_args <- learner_args[(names(learner_args) != "setting_id")]

  # fit final model
  res <- .cv_fit_model(
    self = self,
    private = private,
    train_index = train_index,
    fold_train = fold_train,
    fold_test = fold_test
  )
  outlist <- c(outlist, res)
  return(outlist)
}
