dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

learner <- mlexperiments::LearnerSurvRangerCox
seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_ranger <- expand.grid(
  sample.fraction = seq(0.6, 1, .2),
  min.node.size = seq(1, 5, 4),
  mtry = seq(2, 6, 2),
  num.trees = c(5L, 10L),
  max.depth = seq(1, 5, 4)
)

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
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

fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 10,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv - surv_ranger_cox",
  code = {

    surv_ranger_cox_optimization <- MLCrossValidation$new(
      learner = learner,
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    surv_ranger_cox_optimization$learner_args <- as.list(
      data.table::data.table(param_list_ranger[1, ], stringsAsFactors = FALSE)
    )

    # set data
    surv_ranger_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_ranger_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(10, 6))
    expect_true(inherits(
      x = surv_ranger_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)
