GridOptimizer <- R6::R6Class( # nolint
  inherit = BaseOptimizer,
  classname = "GridOptimizer",
  public = list(
    initialize = function(learner, seed, ncores, ...) {
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      private$strategy <- "grid"
    }
  )
)
