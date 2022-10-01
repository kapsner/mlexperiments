#' @export
MLSurvGlmnetCox <- R6::R6Class( # nolint
  classname = "MLSurvGlmnetCox",
  inherit = MLBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"glmnet\" must be installed to use ",
            "'learner = \"MLSurvGlmnetCox\"."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_higher_better <- TRUE
      self$environment <- "mlexperiments"
      self$cluster_export <- surv_glmnet_cox_ce()
      private$fun_cross_validation <- surv_glmnet_cox_cv
      private$fun_fit <- surv_glmnet_cox_fit
      private$fun_predict <- surv_glmnet_cox_predict
      private$fun_bayesian_scoring_function <- surv_glmnet_cox_bsF
      private$fun_performance_metric <- surv_glmnet_c_index
      self$performance_metric_name <- "C-index"
    }
  )
)


surv_glmnet_cox_ce <- function() {
  c("surv_glmnet_cox_cv", "surv_glmnet_cox_fit",
    ".outsample_row_indices")
}

surv_glmnet_cox_bsF <- function(alpha) { # nolint
  # call to surv_glmnet_cox_cv here with ncores = 1, since the Bayesian search
  # is parallelized already / "FUN is fitted n times in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_glmnet <- surv_glmnet_cox_cv(
    x = x,
    y = y,
    params = list("alpha" = alpha),
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- c(
    list("Score" = bayes_opt_glmnet$mean_cv_metric),
    bayes_opt_glmnet
  )

  return(ret)
}

# tune lambda
surv_glmnet_cox_cv <- function(x, y, params, fold_list, ncores, seed) {
  stopifnot(
    inherits(x = y, what = "Surv"),
    is.list(params),
    length(params) == 1L,
    names(params) == "alpha"
  )

  # from the documentation (help("glmnet::cv.glmnet")):
  # If users would like to cross-validate alpha as well, they should call
  # cv.glmnet with a pre-computed vector foldid, and then use this same
  # fold vector in separate calls to cv.glmnet with different values
  # of alpha.
  glmnet_fids <- .outsample_row_indices(
    fold_list = fold_list,
    training_data = nrow(x),
    type = "glmnet"
  )

  # initialize the parallel backend, if required
  if (ncores > 1L) {
    cl <- register_parallel(ncores)
    on.exit(
      expr = {
        parallel::stopCluster(cl)
        invisible(gc())
      }
    )
    go_parallel <- TRUE
  } else {
    go_parallel <- FALSE
  }

  set.seed(seed)
  # fit the glmnet-cv-model
  cvfit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    family = "cox",
    foldids = glmnet_fids$fold_id,
    type.measure = "C",
    alpha = params$alpha,
    parallel = go_parallel,
    standardize = TRUE
  )

  res <- list(
    "mean_cv_metric" = max(cvfit$cvm),
    "lambda" = cvfit$lambda.min
  )

  return(res)
}

surv_glmnet_cox_fit <- function(x, y, alpha, lambda, seed) {

  set.seed(seed)
  # train final model with a given lambda / alpha
  fit <- glmnet::glmnet(
    x = x,
    y = y,
    family = "cox",
    standardize = TRUE,
    alpha = alpha,
    lambda = lambda
  )
  return(fit)
}

surv_glmnet_cox_predict <- function(model, newdata) {
  # From the docs:
  # Type "response" gives [...] the fitted relative-risk for "cox".
  return(predict(model, newx = newdata, type = "response")[, 1])
}

surv_glmnet_c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
