library(mlbench)
data("PimaIndiansDiabetes2")
dataset <- PimaIndiansDiabetes2 |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:8]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("diabetes")]

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
  desc = "test cv - glm",
  code = {

    glm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerGlm$new(),
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))
    glm_optimization$predict_args <- list(type = "response")
    glm_optimization$performance_metric_args <- list(positive = "pos")
    glm_optimization$performance_metric <- metric("auc")

    # set data
    glm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- glm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 2))
    expect_true(inherits(
      x = glm_optimization$results,
      what = "mlexCV"
    ))
  }
)

test_that(
  desc = "test cv, return models - glm",
  code = {

    glm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerGlm$new(),
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))
    glm_optimization$predict_args <- list(type = "response")
    glm_optimization$performance_metric_args <- list(positive = "pos")
    glm_optimization$performance_metric <- metric("auc")
    glm_optimization$return_models <- TRUE

    # set data
    glm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- glm_optimization$execute()
    expect_type(cv_results, "list")
    expect_true(inherits(
      x = glm_optimization$results$folds[[1]]$model,
      what = "glm"
    ))
  }
)

# ###########################################################################
# %% TUNING
# ###########################################################################

ncores <- 2L

test_that(
  desc = "test bayesian tuner, expect error - glm",
  code = {

    expect_error(mlexperiments::MLTuneParameters$new(
      learner = LearnerGlm$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    ))
  }
)

test_that(
  desc = "test grid, expect error - glm",
  code = {

    expect_error(mlexperiments::MLTuneParameters$new(
      learner = LearnerGlm$new(),
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
  desc = "test nested cv, grid - glm",
  code = {

    expect_error(mlexperiments::MLNestedCV$new(
      learner = LearnerGlm$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    ))
  }
)


test_that(
  desc = "test nested cv, grid - glm",
  code = {

    expect_error(mlexperiments::MLNestedCV$new(
      learner = LearnerGlm$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    ))
  }
)
