#' LearnerLm R6 class
#'
#' @description
#' This learner is a wrapper around [stats::lm()] in order to perform a
#'   linear regression. There is no implementation for tuning
#'   parameters, thus the only experiment to use `LearnerLm` for is
#'   [mlexperiments::MLCrossValidation]
#'
#' @details
#' Implemented methods:
#' \describe{
#' \item{`$fit`}{To fit the model.}
#' \item{`$predict`}{To predict new data with the model.}
#' }
#'
#' @seealso [stats::lm()]
#'
#' @examples
#' LearnerLm$new()
#'
#' @export
#'
LearnerLm <- R6::R6Class( # nolint
  classname = "LearnerLm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerLm` object.
    #'
    #' @details
    #' This learner is a wrapper around [stats::lm()] in order to perform a
    #'   linear regression. There is no implementation for tuning
    #'   parameters, thus the only experiment to use `LearnerLm` for is
    #'   [mlexperiments::MLCrossValidation]
    #'
    #' @return A new `LearnerLm` R6 object.
    #'
    #' @seealso [stats::lm()]
    #'
    #' @examples
    #' LearnerLm$new()
    #'
    initialize = function() {
      super$initialize(
        metric_optimization_higher_better = NULL # unnecessary here
      )
      self$environment <- "mlexperiments"
      private$fun_fit <- lm_fit
      private$fun_predict <- lm_predict

      # there is no optimization step here, so all related functions / fields
      # are set to NULL
      self$cluster_export <- NULL
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

  return(do.call(stats::predict.lm, pred_args))
}
