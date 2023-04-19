#' @title Basic R6 Class for the mlexperiments package
#'
MLBase <- R6::R6Class( # nolint
  classname = "MLBase",
  public = list(
    #' @field results A list. This field is used to store the final results of
    #'   the respective methods.
    results = NULL,

    #' @description
    #' Create a new `MLBase` object.
    #'
    #' @param seed An integer. Needs to be set for reproducibility purposes.
    #' @param ncores An integer to specify the number of cores used for
    #'   parallelization (default: `-1L`).
    #'
    #' @return A new `MLBase` R6 object.
    #'
    #' @examples
    #' \dontrun{
    #' MLBase$new(
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #' }
    #'
    initialize = function(seed, ncores = -1L) {
      stopifnot(
        "`ncores` must be an integer" = is.integer(as.integer(ncores)),
        "`seed` must be an integer" = is.integer(as.integer(seed)),
        "`ncores` must not be `0L`" = ncores != 0L
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

#' @title R6 Class on which the experiment classes are built on
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
        "`learner` must be an R6-class and inherit from `MLLearnerBase`" =
          R6::is.R6(learner) && inherits(learner, "MLLearnerBase")
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
    #' \dontrun{
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
    #' tuner <- MLExperimentsBase$new(
    #'   learner = LearnerKnn$new(),
    #'   seed = 123,
    #'   ncores = 2
    #' )
    #'
    #' # set data
    #' tuner$set_data(
    #'   x = data.matrix(dataset[, -7]),
    #'   y = dataset[, 7]
    #' )
    #' }
    #'
    set_data = function(x, y, cat_vars = NULL) {
      stopifnot(
        "`x` must be a matrix" = inherits(x = x, what = c("matrix", "array")),
        "`x` must contain more than one row" = nrow(x) > 1L,
        "`x` must not be a vector" = !is.vector(x),
        "`cat_vars` must be a character verctor and may contain only \
        column names of `x`" = ifelse(
          test = is.null(cat_vars),
          yes = TRUE,
          no = is.character(cat_vars) && is.atomic(cat_vars) &&
            length(intersect(cat_vars, colnames(x))) == length(cat_vars)
        )
      )
      private$x <- x
      private$y <- y
      private$method_helper$execute_params$cat_vars <- cat_vars
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    method_helper = NULL
  )
)
