#' @import data.table
#' @importFrom data.table ".SD"
#' @importFrom R6 R6Class
NULL

#% https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-
#% variable-note/28887
utils::globalVariables(c("seed", "method_helper", "x", "y"))

mlexperiments_default_options <- list(
  mlexperiments.bayesian.max_init = 50L
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(mlexperiments_default_options) %in% names(op))
  if (any(toset)) options(mlexperiments_default_options[toset])
  invisible()
}
