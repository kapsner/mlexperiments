.method_params_refactor <- function(params, method_helper) {
  if (!is.null(method_helper$params_not_optimized)) {
    params <- c(
      params,
      method_helper$params_not_optimized
    )
  }

  if (!is.null(method_helper$cat_vars)) {
    params <- c(
      params,
      list(cat_vars = method_helper$cat_vars)
    )
  }
  return(params)
}
