#' @title R6 Class to perform hyperparamter tuning experiments
#'
#' @description
#' The `MLTuneParameters` class is used to construct a parameter tuner object
#'   and to perform the tuning of a set of hyperparameters for a specified
#'   machine learning algorithm using either a grid search or a Bayesian
#'   optimization.
#'
#' @details
#' The hyperparameter tuning can be performed with a grid search or a Bayesian
#'   optimization. In both cases, each hyperparameter setting is evaluated in a
#'   k-fold cross-validation on the dataset specified.
#'
#' @seealso [ParBayesianOptimization::bayesOpt()], [splitTools::create_folds()]
#'
#' @examples
#' knn_tuner <- MLTuneParameters$new(
#'   learner = LearnerKnn$new(),
#'   seed = 123,
#'   strategy = "grid",
#'   ncores = 2
#' )
#'
#' @export
#'
MLTuneParameters <- R6::R6Class( # nolint
  classname = "MLTuneParameters",
  inherit = MLExperimentsBase,
  public = list(
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

    #' @description
    #' Create a new `MLTuneParameters` object.
    #'
    #' @param learner An initialized learner object that inherits from class
    #'   `"MLLearnerBase"`.
    #' @param seed An integer. Needs to be set for reproducibility purposes.
    #' @param strategy A character. The strategy to optimize the hyperparameters
    #'   (either `"grid"` or `"bayesian"`).
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #'
    #' @return A new `MLTuneParameters` R6 object.
    #'
    #' @examples
    #' MLTuneParameters$new(
    #'   learner = LearnerKnn$new(),
    #'   seed = 123,
    #'   strategy = "grid",
    #'   ncores = 2
    #' )
    #'
    initialize = function(
      learner,
      seed,
      strategy = c("grid", "bayesian"),
      ncores = -1L
    ) {
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      stopifnot(
        "`private$fun_optim_cv` must not be `NULL`" =
          !is.null(self$learner$.__enclos_env__$private$fun_optim_cv)
      )
      strategy <- match.arg(strategy)
      stopifnot(
        "`learner$cluster_export` must not be `NULL` when using \
        `strategy = 'bayesian'" = ifelse(
          test = strategy == "bayesian",
          yes = !is.null(self$learner$cluster_export),
          no = TRUE
        ),
        !is.null(
          self$learner$.__enclos_env__$private$fun_bayesian_scoring_function
        )
      )
      private$strategy <- strategy

      # init some stuff
      private$method_helper <- list()
      self$split_type <- "stratified"

      private$select_optimizer <- switch(
        EXPR = strategy,
        "grid" = .config_grid_optimizer,
        "bayesian" = .config_bayesian_optimizer
      )

    },

    #' @description
    #' Execute the hyperparameter tuning.
    #'
    #' @param k An integer to define the number of cross-validation folds used
    #'   to tune the hyperparameters.
    #'
    #' @details
    #' All results of the hyperparameter tuning are saved in the field
    #'   `$results` of the `MLTuneParameters` class. After successful execution
    #'   of the parameter tuning, `$results` contains a list with the items
    #'   \describe{
    #'   \item{"summary"}{A data.table with the summarized results (same as
    #'     the returned value of the `execute` method).}
    #'   \item{"best.setting"}{The best setting (according to the learner's
    #'     parameter `metric_optimization_higher_better`) identified during the
    #'     hyperparameter tuning.}
    #'    \item{"bayesOpt"}{The returned value of
    #'      [ParBayesianOptimization::bayesOpt()] (only for `strategy =
    #'     "bayesian"`).}
    #'   }
    #'
    #' @return A `data.table` with the results of the hyperparameter
    #'   optimization. The optimized metric, i.e. the cross-validated evaluation
    #'   metric is given in the column `metric_optim_mean`. More results are
    #'   accessible from the field `$results` of the `MLTuneParameters` class.
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
    #' tuner <- MLTuneParameters$new(
    #'   learner = LearnerKnn$new(),
    #'   seed = 123,
    #'   strategy = "grid",
    #'   ncores = 2
    #' )
    #' tuner$parameter_bounds <- list(k = c(2L, 80L))
    #' tuner$parameter_grid <- expand.grid(
    #'   k = seq(4, 68, 8),
    #'   l = 0,
    #'   test = parse(text = "fold_test$x")
    #' )
    #' tuner$split_type <- "stratified"
    #' tuner$optim_args <- list(
    #'   iters.n = 4,
    #'   kappa = 3.5,
    #'   acq = "ucb"
    #' )
    #'
    #' # set data
    #' tuner$set_data(
    #'   x = data.matrix(dataset[, -7]),
    #'   y = dataset[, 7]
    #' )
    #'
    #' tuner$execute(k = 3)
    #'
    execute = function(k) {
      .tune_init(self, private, k)
      optimizer <- private$select_optimizer(self, private)
      return(.run_tuning(self = self, private = private, optimizer = optimizer))
    }
  ),
  private = list(
    select_optimizer = NULL,
    strategy = NULL,
    tune_params = NULL
  )
)
