library(mlbench)
data("PimaIndiansDiabetes2")
dataset <- PimaIndiansDiabetes2 |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:8]

train_test <- splitTools::partition(
  y = dataset[, get("diabetes")],
  p = c(train = 0.8, test = 0.2),
  type = "stratified",
  seed = seed
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$train, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[train_test$train, get("diabetes")]) - 1L

test_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$test, .SD, .SDcols = feature_cols]
)
test_y <- as.integer(dataset[train_test$test, get("diabetes")]) - 1L

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
  desc = "test predictions, binary - glm",
  code = {

    glm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerGlm$new(),
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))
    glm_optimization$predict_args <- list(type = "response")
    glm_optimization$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
    glm_optimization$performance_metric <- list(
      auc = metric("AUC"), sensitivity = metric("TPR"),
      specificity = metric("TNR")
    )

    # set data
    glm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    glm_optimization$return_models <- TRUE

    cv_results <- glm_optimization$execute()

    # predictions
    preds <- mlexperiments::predictions(
      object = glm_optimization,
      newdata = test_x,
      na.rm = FALSE,
      ncores = 2L,
      type = "response"
    )

    expect_equal(dim(preds), c(79, 7))
    expect_true(inherits(preds, "mlexPredictions"))

    expect_error(
      mlexperiments::predictions(
        object = glm_optimization,
        na.rm = FALSE,
        ncores = 2L
      )
    )

    # performance
    perf <- mlexperiments::performance(
      object = glm_optimization,
      prediction_results = preds,
      y_ground_truth = test_y,
      positive = "1"
    )

    expect_equal(dim(perf), c(5, 4))

    # performance - binary
    perf <- mlexperiments::performance(
      object = glm_optimization,
      prediction_results = preds,
      y_ground_truth = test_y,
      type = "binary",
      positive = "1"
    )

    expect_equal(dim(perf), c(5, 19))
  }
)


# ###########################################################################
# REGRESSION
# ###########################################################################

dataset <- datasets::trees |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:2]

train_test <- splitTools::partition(
  y = dataset[, get("Volume")],
  p = c(train = 0.8, test = 0.2),
  type = "stratified",
  seed = seed
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$train, .SD, .SDcols = feature_cols]
)
train_y <- dataset[train_test$train, get("Volume")]

test_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$test, .SD, .SDcols = feature_cols]
)
test_y <- dataset[train_test$test, get("Volume")]

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
  desc = "test predictions, regression - lm",
  code = {

    lm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerLm$new(),
      fold_list = fold_list,
      seed = seed
    )

    lm_optimization$predict_args <- list(type = "response")
    lm_optimization$performance_metric <- c("rmse", "rmsle", "mse", "msle")

    # set data
    lm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    lm_optimization$return_models <- TRUE

    cv_results <- lm_optimization$execute()

    # predictions
    preds <- mlexperiments::predictions(
      object = lm_optimization,
      newdata = test_x,
      na.rm = FALSE,
      ncores = 2L,
      type = "response"
    )

    expect_equal(dim(preds), c(9, 7))
    expect_true(inherits(preds, "mlexPredictions"))

    expect_error(
      mlexperiments::predictions(
        object = lm_optimization,
        na.rm = FALSE,
        ncores = 2L
      )
    )

    # performance
    perf <- mlexperiments::performance(
      object = lm_optimization,
      prediction_results = preds,
      y_ground_truth = test_y,
      positive = "1"
    )

    expect_equal(dim(perf), c(5, 5))

    # performance - binary
    perf <- mlexperiments::performance(
      object = lm_optimization,
      prediction_results = preds,
      y_ground_truth = test_y,
      type = "regression",
      positive = "1"
    )

    expect_equal(dim(perf), c(5, 9))
  }
)
