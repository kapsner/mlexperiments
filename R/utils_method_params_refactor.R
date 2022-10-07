.method_params_refactor <- function(params, method_helper) {
  if (!is.null(method_helper$execute_params$params_not_optimized)) {
    params <- c(
      params,
      method_helper$execute_params$params_not_optimized
    )
  }

  if (!is.null(method_helper$execute_params$cat_vars)) {
    params <- c(
      params,
      list(cat_vars = method_helper$execute_params$cat_vars)
    )
  }

  # remove duplicates
  params <- params[!kdry::misc_duplicated_by_names(
    params, fromLast = TRUE
  )]

  return(params)
}
