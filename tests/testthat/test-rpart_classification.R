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
train_y <- as.integer(dataset[, get("Class")]) - 1L

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)

options("mlexperiments.bayesian.max_init" = 10L)

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
  desc = "test cv, classification - rpart",
  code = {

    rpart_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerRpart$new(),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    rpart_optimization$learner_args <- list(
      minsplit = 10L,
      maxdepth = 20L,
      cp = 0.03,
      method = "class"
    )
    rpart_optimization$predict_args <- list(type = "class")
    rpart_optimization$performance_metric <- metric("bacc")
    rpart_optimization$return_models <- TRUE

    # set data
    rpart_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- rpart_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = rpart_optimization$results,
      what = "mlexCV"
    ))
  }
)

# ###########################################################################
# %% TUNING
# ###########################################################################


rpart_bounds <- list(
  minsplit = c(2L, 100L),
  cp = c(0.01, 0.1),
  maxdepth = c(2L, 30L)
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)
param_list_rpart <- expand.grid(
  minsplit = seq(2L, 82L, 10L),
  cp = seq(0.01, 0.1, 0.01),
  maxdepth = seq(2L, 30L, 5L)
)

test_that(
  desc = "test bayesian tuner, initGrid, classification - rpart",
  code = {

    rpart_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerRpart$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )

    rpart_optimization$learner_args <- list(method = "class")
    rpart_optimization$parameter_bounds <- rpart_bounds
    rpart_optimization$parameter_grid <- param_list_rpart
    rpart_optimization$split_type <- "stratified"
    rpart_optimization$optim_args <- optim_args

    # set data
    rpart_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results1 <- rpart_optimization$execute(k = 3)
    expect_type(cv_results1, "list")
    expect_true(inherits(
      x = rpart_optimization$results,
      what = "mlexTune"
    ))
  }
)

test_that(
  desc = "test grid tuner, classification - rpart",
  code = {

    rpart_optimization <- mlexperiments::MLTuneParameters$new(
      learner = LearnerRpart$new(),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )

    rpart_optimization$learner_args <- list(method = "class")
    set.seed(seed)
    rand_rows <- sample(seq_len(nrow(param_list_rpart)), 3)
    rpart_optimization$parameter_grid <- param_list_rpart[rand_rows, ]
    rpart_optimization$split_type <- "stratified"

    # set data
    rpart_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- rpart_optimization$execute(k = 3)
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = rpart_optimization$results,
      what = "mlexTune"
    ))
  }
)


# ###########################################################################
# %% NESTED CV
# ###########################################################################


test_that(
  desc = "test nested cv, bayesian, classification - rpart",
  code = {

    rpart_optimization <- mlexperiments::MLNestedCV$new(
      learner = LearnerRpart$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    rpart_optimization$learner_args <- list(method = "class")
    rpart_optimization$parameter_grid <- param_list_rpart
    rpart_optimization$parameter_bounds <- rpart_bounds
    rpart_optimization$split_type <- "stratified"
    rpart_optimization$optim_args <- optim_args

    rpart_optimization$predict_args <- list(type = "class")
    rpart_optimization$performance_metric <- metric("bacc")

    # set data
    rpart_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- rpart_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = rpart_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid, classification - rpart",
  code = {

    rpart_optimization <- mlexperiments::MLNestedCV$new(
      learner = LearnerRpart$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    rpart_optimization$learner_args <- list(method = "class")
    set.seed(seed)
    rand_rows <- sample(seq_len(nrow(param_list_rpart)), 3)
    rpart_optimization$parameter_grid <- param_list_rpart[rand_rows, ]
    rpart_optimization$split_type <- "stratified"

    rpart_optimization$predict_args <- list(type = "class")
    rpart_optimization$performance_metric <- metric("bacc")

    # set data
    rpart_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- rpart_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = rpart_optimization$results,
      what = "mlexCV"
    ))
  }
)
