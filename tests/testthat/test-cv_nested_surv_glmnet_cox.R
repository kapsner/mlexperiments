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
  iters.n = 2L,
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


fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 5,
  type = "stratified",
  seed = seed
)


test_that(
  desc = "test nested cv, bayesian - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_cv <- MLNestedCV$new(
      learner = learner,
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = 2L,
      seed = seed
    )

    surv_glmnet_cox_cv$parameter_bounds <- glmnet_bounds
    surv_glmnet_cox_cv$parameter_grid <- param_list_glmnet
    surv_glmnet_cox_cv$split_type <- "stratified"
    surv_glmnet_cox_cv$split_vector <- split_vector
    surv_glmnet_cox_cv$optim_args <- optim_args

    # set data
    surv_glmnet_cox_cv$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_glmnet_cox_cv$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
    expect_true(inherits(x = surv_glmnet_cox_cv$results, what = "mlexCV"))
  }
)


test_that(
  desc = "test nested cv, grid - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_cv <- MLNestedCV$new(
      learner = learner,
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = 2L,
      seed = seed
    )

    surv_glmnet_cox_cv$parameter_grid <- param_list_glmnet
    surv_glmnet_cox_cv$split_type <- "stratified"
    surv_glmnet_cox_cv$split_vector <- split_vector
    surv_glmnet_cox_cv$optim_args <- optim_args

    # set data
    surv_glmnet_cox_cv$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_glmnet_cox_cv$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
    expect_true(inherits(x = surv_glmnet_cox_cv$results, what = "mlexCV"))
  }
)

