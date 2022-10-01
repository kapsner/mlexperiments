GridOptimizer <- R6::R6Class( # nolint
  inherit = BaseOptimizer,
  classname = "GridOptimizer",
  public = list(
    initialize = function(learner, ...) {
      super$initialize(learner)
      private$strategy <- "grid"
    }
  )
)
