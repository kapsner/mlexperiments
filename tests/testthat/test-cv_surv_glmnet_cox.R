dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvGlmnetCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_glmnet <- expand.grid(
  alpha = seq(0, 1, .2)
)
glmnet_bounds <- list(alpha = c(0., 1.))
optim_args <- list(
  iters.n = 4L,
  kappa = 3.5,
  acq = "ucb",
  otherHalting = list(timeLimit = 30)
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


fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 10,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv - surv_glmnet_cox",
  code = {

    surv_glmnet_cox_cv <- MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      ncores = 2L,
      seed = seed
    )
    surv_glmnet_cox_cv$learner_args = list(alpha = 0.8, lambda = 0.002)

    # set data
    surv_glmnet_cox_cv$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_glmnet_cox_cv$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(10, 3))
    expect_true(inherits(x = surv_glmnet_cox_cv$results, what = "mlexCV"))
  }
)
