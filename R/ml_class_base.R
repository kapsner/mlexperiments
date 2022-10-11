MLBase <- R6::R6Class( # nolint
  classname = "MLBase",
  public = list(
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

MLExperimentsBase <- R6::R6Class( # nolint
  classname = "MLExperimentsBase",
  inherit = MLBase,
  public = list(
    #' @field learner_args A list containing the parameter settings of the
    #'   learner algorithm.
    learner_args = NULL,
    learner = NULL,
    initialize = function(learner, seed, ncores = -1L) {
      super$initialize(seed = seed, ncores = ncores)
      stopifnot(
        # only accept instantiated learners
        R6::is.R6(learner)
      )
      self$learner <- learner
    },
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
