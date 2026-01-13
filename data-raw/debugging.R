library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:180]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Class")]

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
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
if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
}


knn_bounds <- list(k = c(2L, 80L))
optim_args <- list(
  n_iter = ncores,
  kappa = 3.5,
  acq = "ucb"
)
param_list_knn <- expand.grid(
  k = seq(4, 68, 6),
  l = 0,
  prob = FALSE
)

devtools::load_all()

knn_optimization <- mlexperiments::MLTuneParameters$new(
  learner = LearnerKnn$new(),
  strategy = "bayesian",
  ncores = ncores,
  seed = seed
)

knn_optimization$parameter_bounds <- knn_bounds
knn_optimization$parameter_grid <- param_list_knn
knn_optimization$learner_args <- list(test = parse(text = "fold_test$x"))
knn_optimization$split_type <- "stratified"
knn_optimization$optim_args <- optim_args

# set data
knn_optimization$set_data(x = train_x, y = train_y)

cv_results1 <- knn_optimization$execute(k = 3)
