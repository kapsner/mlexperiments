#' @title LearnerGlm R6 class
#'
#' @description
#' This learner is a wrapper around [stats::glm()] in order to perform a
#'   generalized linear regression. There is no implementation for tuning
#'   parameters.
#'
#' @details
#' Can be used with
#' * [mlexperiments::MLCrossValidation]
#'
#' Implemented methods:
#' * `$fit` To fit the model.
#' * `$predict` To predict new data with the model.
#'
#' @seealso [stats::glm()]
#'
#' @examples
#' LearnerGlm$new()
#'
#' @export
#'
LearnerGlm <- R6::R6Class( # nolint
  classname = "LearnerGlm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerGlm` object.
    #'
    #' @details
    #' This learner is a wrapper around [stats::glm()] in order to perform a
    #'   generalized linear regression. There is no implementation for tuning
    #'   parameters, thus the only experiment to use `LearnerGlm` for is
    #'   [mlexperiments::MLCrossValidation].
    #'
    #' @return A new `LearnerGlm` R6 object.
    #'
    #' @seealso [stats::glm()]
    #'
    #' @examples
    #' LearnerGlm$new()
    #'
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
  kwargs <- list(...)

  var_handler <- handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  glm_params <- var_handler$params

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

  return(do.call(stats::predict.glm, pred_args))
}
