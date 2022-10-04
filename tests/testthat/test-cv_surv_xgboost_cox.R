dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvXgboostCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]


param_list_xgboost <- expand.grid(
  objective = "survival:cox",
  eval_metric = "cox-nloglik",
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.04, 0.1, 0.02),
  max_depth = seq(1, 5, 4)
)

split_vector <- splitTools::multi_strata(
  df = dataset[, .SD, .SDcols = surv_cols],
  strategy = "kmeans",
  k = 4
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = setdiff(colnames(dataset), surv_cols[1:2])]
)
train_y <- survival::Surv(
  event = (dataset[, get("status")] |>
             as.character() |>
             as.integer()),
  time = dataset[, get("time")],
  type = "right"
)

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 10,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv - surv_xgboost_cox",
  code = {

    surv_xgboost_cox_cv <- MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      ncores = 2L,
      seed = seed
    )
    surv_xgboost_cox_cv$learner_args <- c(as.list(
      data.table::data.table(param_list_xgboost[1, ], stringsAsFactors = FALSE)
    ),
    nrounds = 45L
    )

    # set data
    surv_xgboost_cox_cv$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_xgboost_cox_cv$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(10, 9))
    expect_true(inherits(x = surv_xgboost_cox_cv$results, what = "mlexCV"))
  }
)
