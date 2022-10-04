dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvXgboostCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_xgboost <- expand.grid(
  objective = "survival:cox",
  eval_metric = "cox-nloglik",
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
)
xgboost_bounds <- list(
  subsample = c(0.2, 1),
  colsample_bytree = c(0.2, 1),
  min_child_weight = c(1L, 10L),
  learning_rate = c(0.1, 0.2),
  max_depth =  c(1L, 10L)
)

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
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

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

test_that(
  desc = "test bayesian tuner, initGrid - surv_xgboost_cox",
  code = {

    surv_xgboost_cox_tuner <- MLTuneParameters$new(
      learner = learner,
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )
    surv_xgboost_cox_tuner$parameter_bounds <- xgboost_bounds
    surv_xgboost_cox_tuner$parameter_grid <- param_list_xgboost
    surv_xgboost_cox_tuner$optim_args <- optim_args

    # create split-strata from training dataset
    surv_xgboost_cox_tuner$split_vector <- split_vector

    # set data
    surv_xgboost_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_xgboost_cox_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(ncores + 10, 15))
    expect_true(inherits(x = surv_xgboost_cox_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner - surv_xgboost_cox",
  code = {

    surv_xgboost_cox_tuner <- MLTuneParameters$new(
      learner = learner,
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    surv_xgboost_cox_tuner$parameter_grid <- param_list_xgboost[random_grid, ]

    # create split-strata from training dataset
    surv_xgboost_cox_tuner$split_vector <- split_vector

    # set data
    surv_xgboost_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_xgboost_cox_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 10))
    expect_true(inherits(x = surv_xgboost_cox_tuner$results, what = "mlexTune"))
  }
)
