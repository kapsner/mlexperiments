#' @export
MLCrossValidation <- R6::R6Class( # nolint
  classname = "MLCrossValidation",
  inherit = MLExperimentsBase,
  public = list(
    fold_list = NULL,
    return_models = NULL,
    performance_metric = NULL,
    performance_metric_name = NULL,
    performance_metric_args = NULL,
    predict_args = NULL,
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
      private$prepare()
      return(.run_cv(self = self, private = private))
    }
  ),
  private = list(
    fun_performance_metric = NULL,
    cv_run_model = function(...) {
      kwargs <- list(...)
      args <- kdry::list.append(
        list(
          self = self,
          private = private
        ),
        kwargs
      )
      do.call(.cv_fit_model, args)
    },
    prepare = function() {
      stopifnot(
        !is.null(private$x), !is.null(private$y),
        !is.null(self$fold_list),
        ifelse(
          test = is.null(self$performance_metric_args),
          yes = TRUE,
          no = is.list(self$performance_metric_args)
        ),
        is.function(self$performance_metric)
      )
      # apply parameter_grid stuff
      .organize_parameter_grid(self = self, private = private)

      stopifnot(
        length(intersect(
          names(private$method_helper$params_not_optimized),
          names(private$execute_params))) == 0L
      )
    }
  )
)
