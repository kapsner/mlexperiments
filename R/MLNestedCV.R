#' @title R6 Class to perform nested cross-validation experiments
#'
#' @description
#' The `MLNestedCV` class is used to construct a nested cross validation object
#'   and to perform a nested cross validation for a specified machine learning
#'   algorithm by performing a hyperparameter optimization with the in-sample
#'   observations of each of the k outer folds and validate them directly on the
#'   out-of-sample observations of the respective fold.
#'
#' @details
#' The `MLNestedCV` class requires to provide a named list of predefined
#'   row indices for the outer cross validation folds, e.g., created with the
#'   function [splitTools::create_folds()]. This list also defines the `k` of
#'   the k-fold cross-validation. Furthermore, a strategy needs to be chosen
#'   ("grid" or "bayesian") for the hyperparameter optimization as well as the
#'   parameter `k_tuning` to define the number of inner cross validation folds.
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
#'
#' fold_list <- splitTools::create_folds(
#'   y = dataset[, 7],
#'   k = 3,
#'   type = "stratified",
#'   seed = 123
#' )
#'
#' cv <- MLNestedCV$new(
#'   learner = LearnerKnn$new(),
#'   strategy = "grid",
#'   fold_list = fold_list,
#'   k_tuning = 3L,
#'   seed = 123,
#'   ncores = 2
#' )
#'
#' # learner args (not optimized)
#' cv$learner_args <- list(
#'   l = 0,
#'   test = parse(text = "fold_test$x")
#' )
#'
#' # parameters for hyperparameter tuning
#' cv$parameter_grid <- expand.grid(
#'   k = seq(4, 16, 8)
#' )
#' cv$split_type <- "stratified"
#'
#' # performance parameters
#' cv$predict_args <- list(type = "response")
#' cv$performance_metric_args <- list(
#'   positive = "1",
#'   negative = "0"
#' )
#' cv$performance_metric <- metric("MMCE")
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
MLNestedCV <- R6::R6Class( # nolint
  classname = "MLNestedCV",
  inherit = MLCrossValidation,
  public = list(
    #' @field strategy A character. The strategy to optimize the hyperparameters
    #'   (either `"grid"` or `"bayesian"`).

    strategy = NULL,
    #' @field parameter_bounds A named list of tuples to define the parameter
    #'   bounds of the Bayesian hyperparameter optimization. For further details
    #'   please see the documentation of the `ParBayesianOptimization` package.
    parameter_bounds = NULL,

    #' @field parameter_grid A matrix with named columns in which each column
    #'   represents a parameter that should be optimized and each row represents
    #'   a specific hyperparameter setting that should be tested throughout the
    #'   procedure. For `strategy = "grid"`, each row of the `parameter_grid` is
    #'   considered as a setting that is evaluated. For `strategy = "bayesian"`,
    #'   the `parameter_grid` is passed further on to the `initGrid` argument of
    #'   the function [ParBayesianOptimization::bayesOpt()] in order to
    #'   initialize the Bayesian process. The maximum rows considered for
    #'   initializing the Bayesian process can be specified with the R option
    #'   `option("mlexperiments.bayesian.max_init")`, which is set to `50L` by
    #'   default.
    parameter_grid = NULL,

    #' @field optim_args A named list of tuples to define the parameter
    #'   bounds of the Bayesian hyperparameter optimization. For further details
    #'   please see the documentation of the `ParBayesianOptimization` package.
    optim_args = NULL,

    #' @field split_type A character. The splitting strategy to construct the
    #'   k cross-validation folds. This parameter is passed further on to the
    #'   function [splitTools::create_folds()] and defaults to `"stratified"`.
    split_type = NULL,

    #' @field split_vector A vector If another criteria than the provided `y`
    #'   should be considered for generating the cross-validation folds, it can
    #'   be defined here. It is important, that a vector of the same length as
    #'   `x` is provided here.
    split_vector = NULL,

    #' @field k_tuning An integer to define the number of cross-validation folds
    #'   used to tune the hyperparameters.
    k_tuning = NULL,

    #' @description
    #' Create a new `MLNestedCV` object.
    #'
    #' @param fold_list A named list of predefined row indices for the cross
    #'   validation folds, e.g., created with the function
    #'   [splitTools::create_folds()].
    #' @param strategy A character. The strategy to optimize the hyperparameters
    #'   (either `"grid"` or `"bayesian"`).
    #' @param learner An initialized learner object that inherits from class
    #'   `"MLLearnerBase"`.
    #' @param k_tuning An integer to define the number of cross-validation folds
    #'   used to tune the hyperparameters.
    #' @param seed An integer. Needs to be set for reproducibility purposes.
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #' @param return_models A logical. If the fitted models should be returned
    #'   with the results (default: `FALSE`).
    #'
    #' @details
    #' The `MLNestedCV` class requires to provide a named list of predefined
    #'   row indices for the outer cross validation folds, e.g., created with
    #'   the function [splitTools::create_folds()]. This list also defines the
    #'   `k` of the k-fold cross-validation. Furthermore, a strategy needs to
    #'   be chosen ("grid" or "bayesian") for the hyperparameter optimization
    #'   as well as the parameter `k_tuning` to define the number of inner
    #'   cross validation folds.
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
    #'
    #' fold_list <- splitTools::create_folds(
    #'   y = dataset[, 7],
    #'   k = 3,
    #'   type = "stratified",
    #'   seed = 123
    #' )
    #'
    #' cv <- MLNestedCV$new(
    #'   learner = LearnerKnn$new(),
    #'   strategy = "grid",
    #'   fold_list = fold_list,
    #'   k_tuning = 3L,
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #'
    initialize = function(
      learner,
      strategy = c("grid", "bayesian"),
      k_tuning,
      fold_list,
      seed,
      ncores = -1L,
      return_models = FALSE
    ) {
      super$initialize(
        learner = learner,
        fold_list = fold_list,
        seed = seed,
        ncores = ncores,
        return_models = return_models
      )
      stopifnot(
        "`fun_optim_cv` must not be `NULL`" =
          !is.null(self$learner$.__enclos_env__$private$fun_optim_cv),
        "`k_tuning` must be an integer >= 3" = as.integer(k_tuning) >= 3L,
        "`k_tuning` must be an integer" = is.integer(as.integer(k_tuning))
      )
      self$k_tuning <- as.integer(k_tuning)
      strategy <- match.arg(strategy)
      self$strategy <- strategy
    },

    #' @description
    #' Execute the nested cross validation.
    #'
    #' @return The function returns a data.table with the results of the nested
    #'   cross validation. More results are accessible from the field `$results`
    #'   of the `MLNestedCV` class.
    #'
    #' @details
    #' All results of the cross validation are saved in the field `$results` of
    #'   the `MLNestedCV` class. After successful execution of the nested cross
    #'   validation, `$results` contains a list with the items:
    #'
    #'   * "results.optimization" A list with the results of the hyperparameter
    #'      optimization.
    #'   * "fold" A list of folds containing the following items for each
    #'   cross validation fold:
    #'     + "fold_ids" A vector with the utilized in-sample row indices.
    #'     + "ground_truth" A vector with the ground truth.
    #'     + "predictions" A vector with the predictions.
    #'     + "learner.args" A list with the arguments provided to the learner.
    #'     + "model" If `return_models = TRUE`, the fitted model.
    #'   * "summary" A data.table with the summarized results (same as
    #'     the returned value of the `execute` method).
    #'   * "performance" A list with the value of the performance metric
    #'     calculated for each of the cross validation folds.
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
    #' cv <- MLNestedCV$new(
    #'   learner = LearnerKnn$new(),
    #'   strategy = "grid",
    #'   fold_list = fold_list,
    #'   k_tuning = 3L,
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #'
    #' # learner args (not optimized)
    #' cv$learner_args <- list(
    #'   l = 0,
    #'   test = parse(text = "fold_test$x")
    #' )
    #'
    #' # parameters for hyperparameter tuning
    #' cv$parameter_grid <- expand.grid(
    #'   k = seq(4, 68, 8)
    #' )
    #' cv$split_type <- "stratified"
    #'
    #' # performance parameters
    #' cv$predict_args <- list(type = "response")
    #' cv$performance_metric_args <- list(
    #'   positive = "1",
    #'   negative = "0"
    #' )
    #' cv$performance_metric <- metric("MMCE")
    #'
    #' # set data
    #' cv$set_data(
    #'   x = data.matrix(dataset[, -7]),
    #'   y = dataset[, 7]
    #' )
    #'
    #' cv$execute()
    #'
    execute = function() {
      private$prepare()
      stopifnot(
        "`strategy` must not be `NULL`" = !is.null(self$strategy),
        "For `strategy = 'bayesian', `parameter_bounds` must be provided.\
        For `strategy = 'grid', `parameter_grid` must be provided" = ifelse(
          test = self$strategy == "bayesian",
          yes = !is.null(self$parameter_bounds),
          no = !is.null(self$parameter_grid)
        ),
        "`execute_params` must not be empty" =
          !is.null(private$method_helper$execute_params)
      )
      return(.run_cv(self = self, private = private))
    }
  ),
  private = list(
    cv_run_model = function(...) {
      kwargs <- list(...)
      args <- kdry::list.append(
        list(
          self = self,
          private = private
        ),
        kwargs
      )
      do.call(.cv_run_nested_model, args)
    }
  )
)
