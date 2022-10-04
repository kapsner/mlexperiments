#' @export
LearnerSurvCoxPHCox <- R6::R6Class( # nolint
  classname = "LearnerSurvCoxPHCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("survival", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"survival\" must be installed to use ",
            "'learner = \"LearnerSurvCoxPHCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_performance_higher_better <- TRUE
      self$environment <- "mlexperiments"
      private$fun_fit <- surv_coxph_cox_fit
      private$fun_predict <- surv_coxph_cox_predict
      private$fun_performance_metric <- surv_coxph_c_index
      self$metric_performance_name <- "C-index"

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
surv_coxph_cox_fit <- function(x, y, ncores, seed, ...) {
  message("Parameter 'ncores' is ignored for learner 'LearnerSurvCoxPHCox'.")
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
  } else {
    cat_vars <- NULL
  }

  x <- .matrix_to_df(matrix = x, cat_vars = cat_vars)

  cox_formula <- as.formula(object = "y ~ .")

  args <- list(
    formula = cox_formula,
    data = x
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(survival::coxph, args)
  return(bst)
}

surv_coxph_cox_predict <- function(model, newdata, ncores, ...) {
  params <- list(...)

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
  } else {
    cat_vars <- NULL
  }

  newdata <- .matrix_to_df(matrix = newdata, cat_vars = cat_vars)

  # type the type of predicted value. Choices are the linear predictor ("lp"),
  # the risk score exp(lp) ("risk"), the expected number of events given the
  # covariates and follow-up time ("expected"), and the terms of the linear
  # predictor ("terms"). The survival probability for a subject is equal
  # to exp(-expected).
  return(predict(model, newdata = newdata, type = "risk"))
}

surv_coxph_c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
