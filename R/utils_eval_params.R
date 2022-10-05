.eval_params <- function(params, env = parent.frame()) {
  is_expression <- vapply(params, is.expression, FUN.VALUE = logical(1L))
  if (sum(is_expression) > 0L) {
    expr_name <- names(params[is_expression])
    for (en in expr_name) {
      params[[en]] <- eval(params[[en]], envir = env)
    }
  }
  return(params)
}
