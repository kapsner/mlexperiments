#' @title validate_fold_equality
#'
#' @description Validate that the same folds were used in two or more
#'   independent experiments.
#'
#' @param experiments A list of experiments.
#'
#' @details
#' This function can be applied to all implemented experiments, i.e.,
#'   [mlexperiments::MLTuneParameters], [mlexperiments::MLCrossValidation], and
#'   [mlexperiments::MLNestedCV]. However, it is required that the list
#'   `experiments` contains only experiments of the same class.
#'
#' @return Writes messages to the console on the result of the comparison.
#'
#' @examples
#' dataset <- do.call(
#'   cbind,
#'   c(sapply(paste0("col", 1:6), function(x) {
#'     rnorm(n = 500)
#'     },
#'     USE.NAMES = TRUE,
#'     simplify = FALSE
#'    ),
#'    list(target = sample(0:1, 500, TRUE))
#' ))
#'
#' fold_list <- splitTools::create_folds(
#'   y = dataset[, 7],
#'   k = 3,
#'   type = "stratified",
#'   seed = 123
#' )
#'
#' # GLM
#' glm_optimization <- mlexperiments::MLCrossValidation$new(
#'   learner = LearnerGlm$new(),
#'   fold_list = fold_list,
#'   seed = 123
#' )
#'
#' glm_optimization$learner_args <- list(family = binomial(link = "logit"))
#' glm_optimization$predict_args <- list(type = "response")
#' glm_optimization$performance_metric_args <- list(
#'   positive = "1",
#'   negative = "0"
#' )
#' glm_optimization$performance_metric <- metric("AUC")
#' glm_optimization$return_models <- TRUE
#'
#' # set data
#' glm_optimization$set_data(
#'   x = data.matrix(dataset[, -7]),
#'   y = dataset[, 7]
#' )
#'
#' glm_cv_results <- glm_optimization$execute()
#'
#' # KNN
#' knn_optimization <- mlexperiments::MLCrossValidation$new(
#'   learner = LearnerKnn$new(),
#'   fold_list = fold_list,
#'   seed = 123
#' )
#' knn_optimization$learner_args <- list(
#'   k = 3,
#'   l = 0,
#'   test = parse(text = "fold_test$x")
#' )
#' knn_optimization$predict_args <- list(type = "prob")
#' knn_optimization$performance_metric_args <- list(
#'   positive = "1",
#'   negative = "0"
#' )
#' knn_optimization$performance_metric <- metric("AUC")
#'
#' # set data
#' knn_optimization$set_data(
#'   x = data.matrix(dataset[, -7]),
#'   y = dataset[, 7]
#' )
#'
#' cv_results_knn <- knn_optimization$execute()
#'
#' # validate folds
#' validate_fold_equality(
#'   list(glm_optimization, knn_optimization)
#' )
#'
#' @export
#'
validate_fold_equality <- function(experiments) {
  stopifnot(
    "`experiments` must be a list and contain more than 1 item" =
      is.list(experiments) || length(experiments) > 1L,
    "All elements of `experiments` must be of type `MLCrossValidation \
    or of type `MLTuneParameters`" =
      all(sapply(
      X = experiments,
      FUN = function(x) {
        inherits(x, "MLCrossValidation")
      }
    )) || all(sapply(
      X = experiments,
      FUN = function(x) {
        inherits(x, "MLTuneParameters")
      }
    ))
  )

  if (inherits(experiments[[1]], "MLCrossValidation")) {
    fold_lists <- "experiments[[%s]]$fold_list"
  } else if (inherits(experiments[[1]], "MLTuneParameters")) {
    fold_lists <- "experiments[[%s]]$results$fold_list"
  }

  for (i in seq_len(length(experiments))) {
    if (i < length(experiments)) {
      test_i <- i + 1L
    } else {
      test_i <- 1L
    }
    message(sprintf("\nTesting for identical folds in %s and %s.", i, test_i))
    stopifnot(
      all(
        "Folds are not identical" = sapply(
          X = names(eval(parse(text = sprintf(fold_lists, "i")))),
          FUN = function(x) {
            all(eval(parse(text = sprintf(fold_lists, "i")))[[x]] ==
                  eval(parse(text = sprintf(fold_lists, "test_i")))[[x]])
          }
        )
      )
    )
  }
}
