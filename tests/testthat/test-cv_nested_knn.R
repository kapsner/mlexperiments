library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

learner <- LearnerKnn
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

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 5,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test nested cv, bayesian - knn",
  code = {

    knn_optimization <- mlexperiments::MLNestedCV$new(
      learner = learner,
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
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

    cv_results <- knn_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
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
      learner = learner,
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    knn_optimization$parameter_grid <- param_list_knn
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    # set data
    knn_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- knn_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 3))
    expect_true(inherits(
      x = knn_optimization$results,
      what = "mlexCV"
    ))
  }
)
