#' @import data.table
#' @importFrom data.table ".SD"
#' @importFrom R6 R6Class
NULL

mlexperiments_default_options <- list(
  # mlexperiments.learner = c(
  #   LearnerKnn$classname, # = "LearnerSurvGlmnetCox"
  #   LearnerRpart$classname
  # ),
  mlexperiments.bayesian.max_init = 50L
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(mlexperiments_default_options) %in% names(op))
  if (any(toset)) options(mlexperiments_default_options[toset])
  invisible()
}
