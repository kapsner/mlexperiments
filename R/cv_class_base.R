#' @export
MLCrossValidation <- R6::R6Class( # nolint
  classname = "MLCrossValidation",
  inherit = MLExperimentsBase,
  public = list(
    fold_list = NULL,
    return_models = NULL,
    initialize = function(
      learner,
      fold_list,
      seed,
      ncores = -1L,
      return_models = FALSE
    ) {
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      stopifnot(is.logical(self$return_models <- return_models))
      stopifnot(is.list(fold_list) && length(fold_list) > 2L)
      self$fold_list <- fold_list
    },
    execute = function() {
      stopifnot(
        !is.null(private$x), !is.null(private$y),
        !is.null(self$fold_list)
      )
      return(.run_cv(self = self, private = private))
    }
  ),
  private = list(
    metric_performance_higher_better = NULL,
    fun_performance_metric = NULL,
    cv_run_model = function(...) {
      kwargs <- list(...)
      args <- c(
        list(
          self = self,
          private = private
        ),
        kwargs
      )
      do.call(.cv_fit_model, args)
    }
  )
)
