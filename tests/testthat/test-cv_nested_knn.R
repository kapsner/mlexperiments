library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:180]

param_list_knn <- expand.grid(
  k = seq(4, 68, 8),
  l = c(0, 1),
  test = parse(text = "fold_test$x")
)

knn_bounds <- list(k = c(2L, 80L))
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
      learner = LearnerKnn$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_knn)), 10)
    knn_optimization$parameter_grid <- param_list_knn[random_grid, ]
    knn_optimization$parameter_bounds <- knn_bounds
    knn_optimization$split_type <- "stratified"
    knn_optimization$optim_args <- optim_args

    knn_optimization$predict_args <- list(type = "response")
    knn_optimization$performance_metric <- metric("bacc")
    knn_optimization$performance_metric_name <- "Balanced accuracy"

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
      learner = LearnerKnn$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_knn)), 10)
    knn_optimization$parameter_grid <- param_list_knn[random_grid, ]
    knn_optimization$split_type <- "stratified"

    knn_optimization$predict_args <- list(type = "response")
    knn_optimization$performance_metric <- metric("bacc")
    knn_optimization$performance_metric_name <- "Balanced accuracy"

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
