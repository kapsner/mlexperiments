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
      ncores <- as.integer(ncores)

      # check available cores
      .check_available_cores(self, private, ncores)
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
    learner = NULL,
    #' @field learner_args A list containing the parameter settings of the
    #'   learner algorithm.
    learner_args = NULL,
    initialize = function(learner, seed, ncores = -1L) {
      stopifnot(
        R6::is.R6Class(learner)
      )
      self$learner <- learner
      super$initialize(seed = seed, ncores = ncores)
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
      private$cat_vars <- cat_vars
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    cat_vars = NULL
  )
)
