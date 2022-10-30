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
  desc = "test validate_fold_equality",
  code = {

    glm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = LearnerGlm$new(),
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))
    glm_optimization$predict_args <- list(type = "response")
    glm_optimization$performance_metric_args <- list(positive = "1")
    glm_optimization$performance_metric <- metric("auc")

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
    knn_optimization$performance_metric_args <- list(positive = "1")
    knn_optimization$performance_metric <- metric("auc")

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
  }
)
