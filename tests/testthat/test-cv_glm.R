dataset <- datasets::iris |>
  data.table::as.data.table() |>
  na.omit()
# to have a binary classification
dataset <- dataset[get("Species") != "virginica", ]

learner <- LearnerGlm
seed <- 123
feature_cols <- colnames(dataset)[1:4]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Species")]

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 5,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv - glm",
  code = {

    glm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))

    # set data
    glm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- glm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 1))
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
      learner = learner,
      fold_list = fold_list,
      seed = seed
    )

    glm_optimization$learner_args <- list(family = binomial(link = "logit"))
    glm_optimization$return_models <- TRUE

    # set data
    glm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- glm_optimization$execute()
    expect_type(cv_results, "list")
    expect_true(inherits(
      x = glm_optimization$results[[1]]$model,
      what = "glm"
    ))
  }
)
