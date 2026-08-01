library(mlbench)
data("BreastCancer")
dataset <- BreastCancer |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[2:10]
to_num <- c(
  "Cl.thickness",
  "Cell.size",
  "Cell.shape",
  "Marg.adhesion",
  "Epith.c.size"
)
dataset[, (to_num) := lapply(.SD, as.numeric), .SDcols = to_num]
to_num = c("Cl.thickness", "Cell.size", "Cell.shape", "Marg.adhesion", "Epith.c.size")
dataset[, (to_num) := lapply(.SD, as.numeric), .SDcols = to_num]

train_test <- splitTools::partition(
  y = dataset[, get("Class")],
  p = c(train = 0.8, test = 0.2),
  type = "stratified",
  seed = seed
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$train, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[train_test$train, get("Class")]) - 1L

test_x <- model.matrix(
  ~ -1 + .,
  dataset[train_test$test, .SD, .SDcols = feature_cols]
)
test_y <- as.integer(dataset[train_test$test, get("Class")]) - 1L

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 5,
  type = "stratified",
  seed = seed
)


# ###########################################################################
# %% CV
# ###########################################################################

test_that(desc = "test validate_fold_equality", code = {
  testthat::skip_if_not_installed("class")
  testthat::skip_if_not_installed("measures")

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
  glm_optimization$performance_metric <- metric("AUC")

  # set data
  glm_optimization$set_data(
    x = train_x,
    y = train_y
  )

  glm_optimization$return_models <- TRUE

  cv_results_glm <- glm_optimization$execute()

  knn_optimization <- mlexperiments::MLCrossValidation$new(
    learner = LearnerKnn$new(),
    fold_list = fold_list,
    seed = seed
  )
  knn_optimization$learner_args <- list(
    k = 3,
    l = 0,
    test = parse(text = "fold_test$x")
  )
  knn_optimization$predict_args <- list(type = "prob")
  knn_optimization$performance_metric_args <- list(
    positive = "1",
    negative = "0"
  )
  knn_optimization$performance_metric <- metric("AUC")

  # set data
  knn_optimization$set_data(
    x = train_x,
    y = train_y
  )

  cv_results_knn <- knn_optimization$execute()

  expect_message(
    object = mlexperiments::validate_fold_equality(
      list(glm_optimization, knn_optimization)
    )
  )
})
