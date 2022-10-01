#' @export
MLExperimentsBase <- R6::R6Class( # nolint
  classname = "MLExperimentsBase",
  public = list(
    learner = NULL,
    #' @field learner_args A list containing the parameter settings of the
    #'   learner algorithm.
    learner_args = NULL,
    results = NULL,
    initialize = function(learner, seed, ncores = -1L) {
      stopifnot(
        R6::is.R6Class(self$learner <- learner),
        is.integer(ncores <- as.integer(ncores)),
        is.integer(private$seed <- as.integer(seed)),
        ncores != 0L
      )

      # check available cores
      .check_available_cores(self, private, ncores)
    },
    set_data = function(x, y) {
      stopifnot(
        inherits(x = x, what = c("matrix", "array")),
        nrow(x) > 1L, !is.vector(x)
      )
      private$x <- x
      private$y <- y
    }
  ),
  private = list(
    x = NULL,
    y = NULL,
    ncores = NULL,
    seed = NULL
  )
)

.check_available_cores <- function(self, private, ncores) {
  available_cores <- parallel::detectCores()

  if (ncores < 0L) {
    private$ncores <- available_cores
  } else if (ncores > 1L) {
    if (ncores > available_cores) {
      message(sprintf(
        paste0(
          "Number of specified cores ('%s') > available cores ('%s')...\n",
          "Setting cores to available cores - 1, i.e. '%s'."),
        ncores, available_cores, I(available_cores - 1L)
      ))
      private$ncores <- available_cores - 1L
    } else {
      private$ncores <- ncores
    }
  } else {
    private$ncores <- 1L
  }
}
