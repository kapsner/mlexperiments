#' R6 Class to perform cross-validation experiments
#'
#' @description
#' The `MLCrossValidation` class is used to construct a cross validation object
#'   and to perform a k-fold cross validation for a specified machine learning
#'   algorithm using one distinct hyperparameter setting.
#'
#' @details
#' The `MLCrossValidation` class requires to provide a named list of predefined
#'   row indices for the cross validation folds, e.g., created with the function
#'   [splitTools::create_folds()]. This list also defines the `k` of the k-fold
#'   cross-validation. When wanting to perform a repeated k-fold cross
#'   validations, just provide a list with all repeated fold definitions, e.g.,
#'   when specifing the argument `m_rep` of [splitTools::create_folds()].
#'
#' @seealso [splitTools::create_folds()]
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
#' fold_list <- splitTools::create_folds(
#'   y = dataset[, 7],
#'   k = 3,
#'   type = "stratified",
#'   seed = 123
#' )
#' cv <- MLCrossValidation$new(
#'   learner = LearnerKnn$new(),
#'   fold_list = fold_list,
#'   seed = 123,
#'   ncores = 2
#' )
#' cv$learner_args <- list(
#'   k = 20,
#'   l = 0,
#'   test = parse(text = "fold_test$x")
#' )
#' cv$predict_args <- list(type = "response")
#' cv$performance_metric <- metric("bacc")
#' cv$performance_metric_name <- "Balanced accuracy"
#'
#' # set data
#' cv$set_data(
#'   x = data.matrix(dataset[, -7]),
#'   y = dataset[, 7]
#' )
#'
#' cv$execute()
#'
#' @export
#'
MLCrossValidation <- R6::R6Class( # nolint
  classname = "MLCrossValidation",
  inherit = MLExperimentsBase,
  public = list(
    fold_list = NULL,
    return_models = NULL,
    performance_metric = NULL,
    performance_metric_name = NULL,
    performance_metric_args = NULL,
    predict_args = NULL,

    #' @description
    #' Create a new `MLCrossValidation` object.
    #'
    #' @param fold_list
    #' @param learner An initialized learner object that inherits from class
    #'   `"MLLearnerBase"`.
    #' @param seed An integer. Needs to be set for reproducibility purposes.
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #' @param return_models
    #'
    #' @details
    #' The `MLCrossValidation` class requires to provide a named list of
    #'   predefined row indices for the cross validation folds, e.g., created
    #'   with the function [splitTools::create_folds()]. This list also defines
    #'   the `k` of the k-fold cross-validation. When wanting to perform a
    #'   repeated k-fold cross validations, just provide a list with all
    #'   repeated fold definitions, e.g., when specifing the argument `m_rep` of
    #'   [splitTools::create_folds()].
    #'
    #' @seealso [splitTools::create_folds()]
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
    #' fold_list <- splitTools::create_folds(
    #'   y = dataset[, 7],
    #'   k = 3,
    #'   type = "stratified",
    #'   seed = 123
    #' )
    #' cv <- MLCrossValidation$new(
    #'   learner = LearnerKnn$new(),
    #'   fold_list = fold_list,
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #'
    initialize = function(
      learner,
      fold_list,
      seed,
      ncores = -1L,
      return_models = FALSE
    ) {
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      stopifnot(is.logical(self$return_models <- return_models))
      stopifnot(is.list(fold_list) && length(fold_list) >= 3L)
      self$fold_list <- fold_list
    },

    #' @description
    #' Execute the cross validation.
    #'
    #' @return The function returns a data.table with the results of the cross
    #'   validation. More results are accessible from the field `$results` of
    #'   the `MLCrossValidation` class.
    #'
    #' @details
    #' All results of the hyperparameter tuning are saved in the field
    #'   `$results` of the `MLTuneParameters` class. After successful execution
    #'   of the parameter tuning, `$results` contains a list with the items
    #'   \describe{
    #'   \item{"fold"}{A list}
    #'   \item{"summary"}{A data.table with the summarized results (same as
    #'     the returned value of the `execute` method).}
    #'   \item{"performance"}{A list}
    #'   }
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
    #' fold_list <- splitTools::create_folds(
    #'   y = dataset[, 7],
    #'   k = 3,
    #'   type = "stratified",
    #'   seed = 123
    #' )
    #' cv <- MLCrossValidation$new(
    #'   learner = LearnerKnn$new(),
    #'   fold_list = fold_list,
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #' cv$learner_args <- list(
    #'   k = 20,
    #'   l = 0,
    #'   test = parse(text = "fold_test$x")
    #' )
    #' cv$predict_args <- list(type = "response")
    #' cv$performance_metric <- metric("bacc")
    #' cv$performance_metric_name <- "Balanced accuracy"
    #'
    #' # set data
    #' cv$set_data(
    #'   x = data.matrix(dataset[, -7]),
    #'   y = dataset[, 7]
    #' )
    #'
    #' cv$execute()
    execute = function() {
      private$prepare()
      return(.run_cv(self = self, private = private))
    }
  ),
  private = list(
    fun_performance_metric = NULL,
    cv_run_model = function(...) {
      kwargs <- list(...)
      args <- kdry::list.append(
        list(
          self = self,
          private = private
        ),
        kwargs
      )
      do.call(.cv_fit_model, args)
    },
    prepare = function() {
      stopifnot(
        !is.null(private$x), !is.null(private$y),
        !is.null(self$fold_list),
        ifelse(
          test = is.null(self$performance_metric_args),
          yes = TRUE,
          no = is.list(self$performance_metric_args)
        ),
        is.function(self$performance_metric)
      )
      # apply parameter_grid stuff
      .organize_parameter_grid(self = self, private = private)

      stopifnot(
        length(intersect(
          names(private$method_helper$params_not_optimized),
          names(private$execute_params))) == 0L
      )
    }
  )
)
