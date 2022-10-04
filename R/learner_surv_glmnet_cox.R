#' @export
LearnerSurvGlmnetCox <- R6::R6Class( # nolint
  classname = "LearnerSurvGlmnetCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"glmnet\" must be installed to use ",
            "'learner = \"LearnerSurvGlmnetCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_optimization_higher_better <- TRUE
      self$metric_performance_higher_better <- TRUE
      self$environment <- "mlexperiments"
      self$cluster_export <- surv_glmnet_cox_ce()
      private$fun_optim_cv <- surv_glmnet_cox_optimization
      private$fun_fit <- surv_glmnet_cox_fit
      private$fun_predict <- surv_glmnet_cox_predict
      private$fun_bayesian_scoring_function <- surv_glmnet_cox_bsF
      private$fun_performance_metric <- surv_glmnet_c_index
      self$metric_performance_name <- "C-index"
    }
  )
)


surv_glmnet_cox_ce <- function() {
  c("surv_glmnet_cox_optimization", "surv_glmnet_cox_fit",
    ".outsample_row_indices")
}

surv_glmnet_cox_bsF <- function(alpha) { # nolint
  # call to surv_glmnet_cox_optimization here with ncores = 1, since the Bayesian search
  # is parallelized already / "FUN is fitted n times in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_glmnet <- surv_glmnet_cox_optimization(
    x = x,
    y = y,
    params = list("alpha" = alpha),
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- c(
    list("Score" = bayes_opt_glmnet$metric_optim_mean),
    bayes_opt_glmnet
  )

  return(ret)
}

# tune lambda
surv_glmnet_cox_optimization <- function(x, y, params, fold_list, ncores, seed) {
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
    "metric_optim_mean" = max(cvfit$cvm),
    "lambda" = cvfit$lambda.min
  )

  return(res)
}

surv_glmnet_cox_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot("lambda" %in% names(kwargs),
            "alpha" %in% names(kwargs))

  set.seed(seed)
  # train final model with a given lambda / alpha
  fit <- glmnet::glmnet(
    x = x,
    y = y,
    family = "cox",
    standardize = TRUE,
    alpha = kwargs$alpha,
    lambda = kwargs$lambda
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
