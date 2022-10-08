#' @export
LearnerLm <- R6::R6Class( # nolint
  classname = "LearnerLm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      super$initialize()
      self$environment <- "mlexperiments"
      private$fun_fit <- lm_fit
      private$fun_predict <- lm_predict
      private$fun_performance_metric <- .metric_mae
      self$metric_performance_name <- "Mean absolute error"

      # there is no optimization step here, so all related functions / fields
      # are set to NULL
      self$cluster_export <- NULL
      self$metric_optimization_higher_better <- NULL
      private$fun_optim_cv <- NULL
      private$fun_bayesian_scoring_function <- NULL
    }
  )
)

# pass parameters as ...
lm_fit <- function(x, y, ncores, seed, ...) {
  message("Parameter 'ncores' is ignored for learner 'LearnerLm'.")
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
  } else {
    cat_vars <- NULL
  }

  x <- kdry::dtr_matrix2df(matrix = x, cat_vars = cat_vars)

  lm_formula <- stats::as.formula(object = "y ~ .")

  args <- list(
    formula = lm_formula,
    data = x
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(stats::lm, args)
  return(bst)
}

lm_predict <- function(model, newdata, ncores, ...) {
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
  } else {
    cat_vars <- NULL
  }

  newdata <- kdry::dtr_matrix2df(matrix = newdata, cat_vars = cat_vars)

  return(stats::predict.lm(model, newdata = newdata, type = "response"))
}
