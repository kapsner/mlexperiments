dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- MLSurvGlmnetCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_glmnet <- expand.grid(
  alpha = seq(0, 1, .2)
)
glmnet_bounds <- list(alpha = c(0., 1.))
optim_args <- list(
  iters.n = 4L,
  kappa = 3.5,
  acq = "ucb",
  otherHalting = list(timeLimit = 30)
)

split_vector <- splitTools::multi_strata(
  df = dataset[, .SD, .SDcols = surv_cols],
  strategy = "kmeans",
  k = 4
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = setdiff(colnames(dataset), surv_cols[1:2])]
)
train_y <- survival::Surv(
  event = (dataset[, get("status")] |>
             as.character() |>
             as.integer()),
  time = dataset[, get("time")],
  type = "right"
)

test_that(
  desc = "test bayesian tuner, initGrid - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_tuner <- MLTuneParameters$new(
      learner = learner,
      strategy = "bayesian",
      ncores = 2L,
      seed = seed
    )
    surv_glmnet_cox_tuner$parameter_bounds <- glmnet_bounds
    surv_glmnet_cox_tuner$parameter_grid <- param_list_glmnet
    surv_glmnet_cox_tuner$optim_args <- optim_args

    # create split-strata from training dataset
    surv_glmnet_cox_tuner$split_vector <- split_vector

    # set data
    surv_glmnet_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_glmnet_cox_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 11))
    expect_true(inherits(x = surv_glmnet_cox_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test bayesian tuner, initPoints - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_tuner <- MLTuneParameters$new(
      learner = learner,
      strategy = "bayesian",
      ncores = 2L,
      seed = seed
    )
    surv_glmnet_cox_tuner$parameter_bounds <- glmnet_bounds
    surv_glmnet_cox_tuner$optim_args <- optim_args

    # create split-strata from training dataset
    surv_glmnet_cox_tuner$split_vector <- split_vector

    # set data
    surv_glmnet_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_glmnet_cox_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(8, 11))
    expect_true(inherits(x = surv_glmnet_cox_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_tuner <- MLTuneParameters$new(
      learner = learner,
      strategy = "grid",
      ncores = 2L,
      seed = seed
    )
    surv_glmnet_cox_tuner$parameter_grid <- param_list_glmnet

    # create split-strata from training dataset
    surv_glmnet_cox_tuner$split_vector <- split_vector

    # set data
    surv_glmnet_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_glmnet_cox_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(6, 4))
    expect_true(inherits(x = surv_glmnet_cox_tuner$results, what = "mlexTune"))
  }
)
