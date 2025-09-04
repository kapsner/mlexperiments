library(mlbench)
data("BostonHousing")
dataset <- BostonHousing |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:13]
cat_vars <- "chas"

train_x <- data.matrix(
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("medv")]

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
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
    lm_optimization$performance_metric <- metric("MSE")

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y,
      cat_vars = cat_vars
    )

    cv_results <- lm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 2))
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
    lm_optimization$performance_metric <- metric("MSE")

    lm_optimization$return_models <- TRUE

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y,
      cat_vars = cat_vars
    )

    cv_results <- lm_optimization$execute()
    expect_type(cv_results, "list")
    expect_true(inherits(
      x = lm_optimization$results$folds[[1]]$model,
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
  desc = "test nested cv, bayesian - lm",
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


# ###########################################################################
# %% OTHER ERRORS
# ###########################################################################


test_that(
  desc = "test lm, error when no cat_vars in prediction",
  code = {

    lm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerLm$new(),
      fold_list = fold_list,
      seed = seed
    )
    lm_optimization$predict_args <- list(type = "response")
    lm_optimization$performance_metric <- metric("MSE")
    lm_optimization$learner_args <- list(cat_vars = cat_vars)
    lm_optimization$return_models <- TRUE

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lm_optimization$execute()

    expect_error(
      expect_warning(
        mlexperiments::predictions(
          object = lm_optimization,
          newdata = test_x
        )
      )
    )
  }
)

test_that(
  desc = "test lm, error when cat_vars in prediction",
  code = {

    lm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerLm$new(),
      fold_list = fold_list,
      seed = seed
    )
    lm_optimization$predict_args <- list(type = "response")
    lm_optimization$performance_metric <- metric("MSE")

    lm_optimization$return_models <- TRUE

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lm_optimization$execute()

    expect_error(
      mlexperiments::predictions(
        object = lm_optimization,
        newdata = test_x,
        cat_vars = cat_vars
      )
    )
  }
)
