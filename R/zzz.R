#' @import data.table
#' @importFrom data.table ".SD"
NULL

mlexperiments_default_options <- list(
  mlexperiments.learner = c(
    LearnerSurvGlmnetCox$classname # = "LearnerSurvGlmnetCox"
  ),
  mlexperiments.optim.xgb.nrounds = 5000L,
  mlexperiments.optim.xgb.early_stopping_rounds = 500L,
  mlexperiments.bayesian.max_init = 50L,
  mlexperiments.xgb.print_every_n = 50L
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(mlexperiments_default_options) %in% names(op))
  if (any(toset)) options(mlexperiments_default_options[toset])
  invisible()
}
