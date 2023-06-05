library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:180]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Class")]

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
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
if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
}

# ###########################################################################
# %% CV
# ###########################################################################

test_that(
  desc = "test cv - knn",
  code = {

    knn_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerKnn$new(),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    knn_optimization$learner_args <- list(
      k = 20,
      l = 0,
      test = parse(text = "fold_test$x")
    )
    knn_optimization$predict_args <- list(type = "response")
    knn_optimization$performance_metric <- metric("bacc")
    knn_optimization$return_models <- TRUE

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 4))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexCV"
    ))
  }
)

# ###########################################################################
# %% TUNING
# ###########################################################################


knn_bounds <- list(k = c(2L, 80L))
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)
param_list_knn <- expand.grid(
  k = seq(4, 20, 8),
  l = 0,
  test = parse(text = "fold_test$x")
)

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
      l = 0,
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
  desc = "test bayesian tuner, initPoints - LearnerKnn",
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
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))
  }
)


test_that(
  desc = "test grid tuner - knn",
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
    expect_equal(dim(cv_results), c(3, 4))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexTune"
    ))
  }
)


# ###########################################################################
# %% NESTED CV
# ###########################################################################


test_that(
  desc = "test nested cv, bayesian - knn",
  code = {

    knn_optimization <- mlexperiments::MLNestedCV$new(
      learner = LearnerKnn$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_grid <- param_list_knn
    knn_optimization$parameter_bounds <- knn_bounds
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    knn_optimization$predict_args <- list(type = "response")
    knn_optimization$performance_metric <- metric("bacc")

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 4))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - knn",
  code = {

    knn_optimization <- mlexperiments::MLNestedCV$new(
      learner = LearnerKnn$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_grid <- param_list_knn
    knn_optimization$split_type <- "stratified"

    knn_optimization$predict_args <- list(type = "response")
    knn_optimization$performance_metric <- metric("bacc")

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 4))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexCV"
    ))
  }
)
