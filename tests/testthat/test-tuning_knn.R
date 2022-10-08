library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:180]

param_list_knn <- expand.grid(
  k = seq(4, 74, 10),
  l = 4,
  test = parse(text = "fold_test$x")
)

knn_bounds <- list(k = c(1L, 100L))
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

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Class")]

test_that(
  desc = "test bayesian tuner, initGrid - knn",
  code = {

    knn_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerKnn$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_bounds <- knn_bounds
    knn_optimization$parameter_grid <- param_list_knn
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results1 <- knn_optimization$execute(k = 3)
    expect_type(cv_results1, "list")
    expect_equal(dim(cv_results1), c(8, 11))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))

    # check if learner_args yield same results
    knn_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerKnn$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_bounds <- knn_bounds
    knn_optimization$parameter_grid <- expand.grid(
      list(k = param_list_knn[, 1])
    )
    knn_optimization$learner_args <- list(
      l = 4,
      test = parse(text = "fold_test$x")
    )
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results2 <- knn_optimization$execute(k = 3)
    expect_type(cv_results2, "list")
    expect_equal(dim(cv_results2), c(8, 11))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))

    expect_equal(
      cv_results1[, .SD, .SDcols = !"Elapsed"],
      cv_results2[, .SD, .SDcols = !"Elapsed"]
    )
  }
)


test_that(
  desc = "test bayesian tuner, initPoints - surv_glmnet_cox",
  code = {

    knn_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerKnn$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_bounds <- knn_bounds
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute(k = 3)
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(4, 10))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))
  }
)


test_that(
  desc = "test grid tuner - surv_glmnet_cox",
  code = {

    knn_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerKnn$new(),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_grid <- param_list_knn
    knn_optimization$split_type <- "stratified"

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute(k = 3)
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(8, 4))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))
  }
)
