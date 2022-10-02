#' @export
MLNestedCV <- R6::R6Class( # nolint
  classname = "MLNestedCV",
  inherit = MLCrossValidation,
  public = list(
    strategy = NULL,
    parameter_bounds = NULL,
    parameter_grid = NULL,
    optim_args = NULL,
    split_type = NULL,
    split_vector = NULL,
    k_tuning = NULL,
    initialize = function(
      learner,
      strategy = c("grid", "bayesian"),
      k_tuning,
      fold_list,
      seed,
      ncores = -1L,
      return_models = FALSE
    ) {
      super$initialize(
        learner = learner,
        fold_list = fold_list,
        seed = seed,
        ncores = ncores,
        return_models = return_models
      )
      stopifnot(is.integer(self$k_tuning <- as.integer(k_tuning)))
      strategy <- match.arg(strategy)
      self$strategy <- strategy
    },
    execute = function() {
      stopifnot(
        !is.null(private$x), !is.null(private$y),
        !is.null(self$fold_list),
        !is.null(self$strategy),
        ifelse(
          test = self$strategy == "bayesian",
          yes = !is.null(self$parameter_bounds),
          no = !is.null(self$parameter_grid)
        )
      )
      return(.run_cv(self = self, private = private))
    }
  ),
  private = list(
    init_learner = NULL,
    cv_run_model = function(...) {
      kwargs <- list(...)
      args <- c(
        list(
          self = self,
          private = private
        ),
        kwargs
      )
      do.call(.cv_run_nested_model, args)
    }
  )
)
