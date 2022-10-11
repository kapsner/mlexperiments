dataset <- datasets::trees |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:2]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Volume")]

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 5,
  type = "stratified",
  seed = seed
)

# ###########################################################################
# %% CV
# ###########################################################################

test_that(
  desc = "test cv - lm",
  code = {

    lm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerLm$new(),
      fold_list = fold_list,
      seed = seed
    )
    lm_optimization$predict_args <- list(type = "response")
    lm_optimization$performance_metric <- metric("mse")
    lm_optimization$performance_metric_name <- "Mean squared error"

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 2))
    expect_true(inherits(
      x = lm_optimization$results,
      what = "mlexCV"
    ))
  }
)

test_that(
  desc = "test cv, return models - lm",
  code = {

    lm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerLm$new(),
      fold_list = fold_list,
      seed = seed
    )
    lm_optimization$predict_args <- list(type = "response")
    lm_optimization$performance_metric <- metric("mse")
    lm_optimization$performance_metric_name <- "Mean squared error"

    lm_optimization$return_models <- TRUE

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lm_optimization$execute()
    expect_type(cv_results, "list")
    expect_true(inherits(
      x = lm_optimization$results[[1]]$model,
      what = "lm"
    ))
  }
)

# ###########################################################################
# %% TUNING
# ###########################################################################

ncores <- 2L

test_that(
  desc = "test bayesian tuner, expect error - lm",
  code = {

    expect_error(mlexperiments::MLTuneParameters$new(
      learner = LearnerLm$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    ))
  }
)

test_that(
  desc = "test grid, expect error - lm",
  code = {

    expect_error(mlexperiments::MLTuneParameters$new(
      learner = LearnerLm$new(),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    ))
  }
)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(
  desc = "test nested cv, grid - lm",
  code = {

    expect_error(mlexperiments::MLNestedCV$new(
      learner = LearnerLm$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    ))
  }
)


test_that(
  desc = "test nested cv, grid - lm",
  code = {

    expect_error(mlexperiments::MLNestedCV$new(
      learner = LearnerLm$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    ))
  }
)
