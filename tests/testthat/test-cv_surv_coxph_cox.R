dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvCoxPHCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

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

fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 10,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv - surv_coxph_cox",
  code = {

    surv_coxph_cox_optimization <- MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      ncores = -1L,
      seed = seed
    )

    # set data
    surv_coxph_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_coxph_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(10, 1))
    expect_true(inherits(x = surv_coxph_cox_optimization$results, what = "mlexCV"))
  }
)
