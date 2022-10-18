#' Basic R6 Class for the mlexperiments package
#'
MLBase <- R6::R6Class( # nolint
  classname = "MLBase",
  public = list(
    #' @field results A list. This field is used to store the final results of
    #'   the respective methods.
    results = NULL,

    initialize = function(seed, ncores = -1L) {
      stopifnot(
        is.integer(as.integer(ncores)),
        is.integer(as.integer(seed)),
        ncores != 0L
      )
      private$seed <- as.integer(seed)

      # check available cores
      private$ncores <- kdry::pch_check_available_cores(as.integer(ncores))
    }
  ),
  private = list(
    ncores = NULL,
    seed = NULL
  )
)

#' R6 Class for the mlexperiments of the package
#'
MLExperimentsBase <- R6::R6Class( # nolint
  classname = "MLExperimentsBase",
  inherit = MLBase,
  public = list(
    #' @field learner_args A list containing the parameter settings of the
    #'   learner algorithm.
    learner_args = NULL,

    #' @field learner An initialized learner object that inherits from class
    #'   `"MLLearnerBase"`.
    learner = NULL,

    #' @description
    #' Create a new `MLExperimentsBase` object.
    #'
    #' @param learner An initialized learner object that inherits from class
    #'   `"MLLearnerBase"`.
    #' @param seed An integer. Needs to be set for reproducibility purposes.
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #'
    #' @return A new `MLExperimentsBase` R6 object.
    #'
    #' @examples
    #' \dontrun{
    #' MLExperimentsBase$new(
    #'   learner = LearnerKnn$new(),
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #' }
    #'
    initialize = function(learner, seed, ncores = -1L) {
      super$initialize(seed = seed, ncores = ncores)
      stopifnot(
        # only accept instantiated learners
        R6::is.R6(learner),
        inherits(learner, "MLLearnerBase")
      )
      self$learner <- learner
    },

    #' @description
    #' Set the data for the experiment.
    #'
    #' @param x A matrix with the training data.
    #' @param y A vector with the target.
    #' @param cat_vars A character vector with the column names of variables
    #'   that should be treated as categorical features (if applicable /
    #'   supported by the respective alogrithm).
    #'
    #' @return The function has no return value. It internally performs quality
    #'   checks on the provided data and, if passed, defines private fields of
    #'   the R6 class.
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
    set_data = function(x, y, cat_vars = NULL) {
      stopifnot(
        inherits(x = x, what = c("matrix", "array")),
        nrow(x) > 1L, !is.vector(x),
        ifelse(
          test = is.null(cat_vars),
          yes = TRUE,
          no = is.character(cat_vars) && is.atomic(cat_vars) &&
            intersect(cat_vars, colnames(x)) == cat_vars
        )
      )
      private$x <- x
      private$y <- y
      private$method_helper$cat_vars <- cat_vars
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    method_helper = NULL
  )
)
