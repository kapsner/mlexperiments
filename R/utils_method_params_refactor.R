.method_params_refactor <- function(params, method_helper) {
  params <- kdry::list.append(
    params,
    method_helper$execute_params$params_not_optimized
  )
  params <- kdry::list.append(
    params,
    method_helper$execute_params["cat_vars"]
  )
  return(params)
}
