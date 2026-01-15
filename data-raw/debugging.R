# library(mlbench)
# data("DNA")
# dataset <- DNA |>
#   data.table::as.data.table() |>
#   na.omit()

# seed <- 123
# feature_cols <- colnames(dataset)[1:180]

# train_x <- model.matrix(
#   ~ -1 + .,
#   dataset[, .SD, .SDcols = feature_cols]
# )
# train_y <- dataset[, get("Class")]

# fold_list <- splitTools::create_folds(
#   y = train_y,
#   k = 3,
#   type = "stratified",
#   seed = seed
# )


# ncores <- ifelse(
#   test = parallel::detectCores() > 4,
#   yes = 4L,
#   no = ifelse(
#     test = parallel::detectCores() < 2L,
#     yes = 1L,
#     no = parallel::detectCores()
#   )
# )
# if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
#   # on cran
#   ncores <- 2L
# }


# knn_bounds <- list(k = c(2L, 80L))
# optim_args <- list(
#   n_iter = ncores,
#   kappa = 3.5,
#   acq = "ucb"
# )
# param_list_knn <- expand.grid(
#   k = seq(4, 68, 6),
#   l = 0,
#   prob = FALSE
# )

# devtools::load_all()

# knn_optimization <- mlexperiments::MLTuneParameters$new(
#   learner = LearnerKnn$new(),
#   strategy = "bayesian",
#   ncores = ncores,
#   seed = seed
# )

# knn_optimization$parameter_bounds <- knn_bounds
# knn_optimization$parameter_grid <- param_list_knn
# knn_optimization$learner_args <- list(test = parse(text = "fold_test$x"))
# knn_optimization$split_type <- "stratified"
# knn_optimization$optim_args <- optim_args

# # set data
# knn_optimization$set_data(x = train_x, y = train_y)

# cv_results1 <- knn_optimization$execute(k = 3)



devtools::load_all()

library(mllrnrs)
library(mlbench)

data("PimaIndiansDiabetes2")
dataset <- PimaIndiansDiabetes2 |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:8]

param_list_glmnet <- expand.grid(
  alpha = seq(0, 1, 0.05)
)

if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
} else {
  ncores <- ifelse(
    test = parallel::detectCores() > 4,
    yes = 4L,
    no = ifelse(
      test = parallel::detectCores() < 2L,
      yes = 1L,
      no = parallel::detectCores()
    )
  )
}

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[, get("diabetes")]) - 1L

options("mlexperiments.bayesian.max_init" = 4L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)


options("mlexperiments.bayesian.max_init" = 4L)
options("mlexperiments.optim.lgb.nrounds" = 100L)
options("mlexperiments.optim.lgb.early_stopping_rounds" = 10L)


# param_list_lightgbm <- expand.grid(
#   bagging_fraction = seq(0.6, 1, .2),
#   feature_fraction = seq(0.6, 1, .2),
#   min_data_in_leaf = seq(2, 10, 2),
#   learning_rate = seq(0.1, 0.2, 0.1),
#   num_leaves = seq(2, 20, 4),
#   max_depth = -1L,
#   verbose = -1L
# )

# lightgbm_bounds <- list(
#   bagging_fraction = c(0.2, 1),
#   feature_fraction = c(0.2, 1),
#   min_data_in_leaf = c(2L, 12L),
#   learning_rate = c(0.1, 0.2),
#   num_leaves =  c(2L, 20L)
# )
# optim_args <- list(
#   n_iter = ncores,
#   kappa = 3.5,
#   acq = "ucb"
# )

# lightgbm_optimizer <- mlexperiments::MLNestedCV$new(
#   learner = mllrnrs::LearnerLightgbm$new(
#     metric_optimization_higher_better = FALSE
#   ),
#   strategy = "bayesian",
#   fold_list = fold_list,
#   k_tuning = 3L,
#   ncores = ncores,
#   seed = seed
# )

# lightgbm_optimizer$parameter_bounds <- lightgbm_bounds
# lightgbm_optimizer$parameter_grid <- param_list_lightgbm
# lightgbm_optimizer$split_type <- "stratified"
# lightgbm_optimizer$optim_args <- optim_args

# lightgbm_optimizer$learner_args <- list(
#   objective = "binary",
#   metric = "binary_logloss",
#   cat_vars = c("pregnant", "pedigree")
# )
# lightgbm_optimizer$performance_metric_args <- list(
#   positive = "1",
#   negative = "0"
# )
# lightgbm_optimizer$performance_metric <- mlexperiments::metric("auc")

# # set data
# lightgbm_optimizer$set_data(
#   x = train_x,
#   y = train_y
# )

# cv_results <- lightgbm_optimizer$execute()



param_list_xgboost <- expand.grid(
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
)

ncores <- 2L

options("mlexperiments.bayesian.max_init" = 2L)
options("mlexperiments.optim.xgb.nrounds" = 20L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 5L)

xgboost_optimizer <- mlexperiments::MLNestedCV$new(
  learner = mllrnrs::LearnerXgboost$new(
    metric_optimization_higher_better = FALSE
  ),
  strategy = "grid",
  fold_list = fold_list,
  k_tuning = 3L,
  ncores = ncores,
  seed = seed
)
set.seed(seed)
random_grid <- sample(seq_len(nrow(param_list_xgboost)), 3)
xgboost_optimizer$parameter_grid <-
  param_list_xgboost[random_grid, ]
xgboost_optimizer$split_type <- "stratified"

xgboost_optimizer$learner_args <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)
xgboost_optimizer$performance_metric_args <- list(
  positive = "1",
  negative = "0"
)
xgboost_optimizer$performance_metric <- mlexperiments::metric("auc")

# set data
xgboost_optimizer$set_data(
  x = train_x,
  y = train_y
)

cv_results <- xgboost_optimizer$execute()
