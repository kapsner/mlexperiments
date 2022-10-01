#' @import data.table
#' @importFrom data.table ".SD"
#' @importFrom magrittr "%>%"
NULL

mlexperiments_default_options <- list(
  mlexperiments.learner = c(
    MLSurvGlmnetCox$classname # = "MLSurvGlmnetCox"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(mlexperiments_default_options) %in% names(op))
  if (any(toset)) options(mlexperiments_default_options[toset])
  invisible()
}
