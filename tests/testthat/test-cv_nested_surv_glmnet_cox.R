dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvGlmnetCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_glmnet <- expand.grid(
  alpha = seq(0, 1, .2)
)
glmnet_bounds <- list(alpha = c(0., 1.))


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


fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 5,
  type = "stratified",
  seed = seed
)


test_that(
  desc = "test nested cv, bayesian - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_optimization <- MLNestedCV$new(
      learner = learner,
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    surv_glmnet_cox_optimization$parameter_bounds <- glmnet_bounds
    surv_glmnet_cox_optimization$parameter_grid <- param_list_glmnet
    surv_glmnet_cox_optimization$split_type <- "stratified"
    surv_glmnet_cox_optimization$split_vector <- split_vector
    surv_glmnet_cox_optimization$optim_args <- optim_args

    # set data
    surv_glmnet_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_glmnet_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
    expect_true(inherits(
      x = surv_glmnet_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_optimization <- MLNestedCV$new(
      learner = learner,
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    surv_glmnet_cox_optimization$parameter_grid <- param_list_glmnet
    surv_glmnet_cox_optimization$split_type <- "stratified"
    surv_glmnet_cox_optimization$split_vector <- split_vector
    surv_glmnet_cox_optimization$optim_args <- optim_args

    # set data
    surv_glmnet_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_glmnet_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
    expect_true(inherits(
      x = surv_glmnet_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)
