library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

learner <- LearnerKnn
seed <- 123
feature_cols <- colnames(dataset)[1:180]

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
  desc = "test cv - knn",
  code = {

    knn_optimization <- mlexperiments::MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      seed = seed
    )
    knn_optimization$learner_args <- list(
      k = 20,
      l = 0,
      test = parse(text = "fold_test$x")
    )

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
