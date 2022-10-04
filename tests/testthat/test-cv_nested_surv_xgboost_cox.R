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

fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 5,
  type = "stratified",
  seed = seed
)


test_that(
  desc = "test nested cv, bayesian - surv_xgboost_cox",
  code = {

    surv_xgboost_cox_optimization <- MLNestedCV$new(
      learner = learner,
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    surv_xgboost_cox_optimization$parameter_bounds <- xgboost_bounds
    surv_xgboost_cox_optimization$parameter_grid <- param_list_xgboost
    surv_xgboost_cox_optimization$split_type <- "stratified"
    surv_xgboost_cox_optimization$split_vector <- split_vector
    surv_xgboost_cox_optimization$optim_args <- optim_args

    # set data
    surv_xgboost_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_xgboost_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 9))
    expect_true(inherits(
      x = surv_xgboost_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - surv_xgboost_cox",
  code = {

    surv_xgboost_cox_optimization <- MLNestedCV$new(
      learner = learner,
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    surv_xgboost_cox_optimization$parameter_grid <-
      param_list_xgboost[random_grid, ]
    surv_xgboost_cox_optimization$split_type <- "stratified"
    surv_xgboost_cox_optimization$split_vector <- split_vector
    surv_xgboost_cox_optimization$optim_args <- optim_args

    # set data
    surv_xgboost_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_xgboost_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 9))
    expect_true(inherits(
      x = surv_xgboost_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)
