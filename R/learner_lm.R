#' @title LearnerLm R6 class
#'
#' @description
#' This learner is a wrapper around [stats::lm()] in order to perform a
#'   linear regression. There is no implementation for tuning
#'   parameters.
#'
#' @details
#' Can be used with
#' * mlexperiments::MLCrossValidation
#'
#' @details
#' Implemented methods:
#' * `$fit` To fit the model.
#' * `$predict` To predict new data with the model.
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
  kwargs <- list(...)

  var_handler <- handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  lm_params <- var_handler$params

  lm_formula <- stats::as.formula(object = "y ~ .")

  args <- kdry::list.append(
    list(
      formula = lm_formula,
      data = kdry::dtr_matrix2df(matrix = x, cat_vars = cat_vars)
    ),
    lm_params
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(stats::lm, args)
  return(bst)
}

lm_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)

  var_handler <- handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  params <- var_handler$params

  pred_args <- kdry::list.append(
    list(
      object = model,
      newdata = kdry::dtr_matrix2df(matrix = newdata, cat_vars = cat_vars)
    ),
    params
  )

  return(do.call(stats::predict.lm, pred_args))
}
