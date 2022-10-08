#' @export
LearnerGlm <- R6::R6Class( # nolint
  classname = "LearnerGlm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      super$initialize(
        metric_optimization_higher_better = NULL # unnecessary here
      )
      self$environment <- "mlexperiments"
      private$fun_fit <- glm_fit
      private$fun_predict <- glm_predict

      # there is no optimization step here, so all related functions / fields
      # are set to NULL
      self$cluster_export <- NULL
      private$fun_optim_cv <- NULL
      private$fun_bayesian_scoring_function <- NULL
    }
  )
)

# pass parameters as ...
glm_fit <- function(x, y, ncores, seed, ...) {
  message("Parameter 'ncores' is ignored for learner 'LearnerGlm'.")
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
    glm_params <- params[names(params) != "cat_vars"]
  } else {
    cat_vars <- NULL
    glm_params <- params
  }

  x <- kdry::dtr_matrix2df(matrix = x, cat_vars = cat_vars)

  glm_formula <- stats::as.formula(object = "y ~ .")

  args <- kdry::list.append(
    list(
      formula = glm_formula,
      data = x
    ),
    glm_params
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(stats::glm, args)
  return(bst)
}

glm_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)

  if ("cat_vars" %in% names(kwargs)) {
    cat_vars <- kwargs[["cat_vars"]]
  } else {
    cat_vars <- NULL
  }

  pred_args <- kdry::list.append(
    list(
      object = model,
      newdata = kdry::dtr_matrix2df(matrix = newdata, cat_vars = cat_vars)
    ),
    kwargs
  )

  return(do.call(stats::predict.glm, pred_args))
}
