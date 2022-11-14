#' @title handle_cat_vars
#'
#' @description Helper function to handle categorical variables
#'
#' @details
#' This function is a utility function to separate the list element with the
#'   names of the categorical variables from the key word arguments list to
#'   be passed further on to [kdry::dtr_matrix2df()].
#'
#' @param kwargs A list with key word arguments.
#'
#' @return Returns a list with two elements:
#'   * `params` The key word arguments without `cat_vars`.
#'   * `cat_vars` The vector `cat_vars`.
#'
#' @seealso [kdry::dtr_matrix2df()]
#'
#' @examples
#' handle_cat_vars(list(cat_vars = c("a", "b", "c"), arg1 = 1, arg2 = 2))
#'
#' @export
#'
handle_cat_vars <- function(kwargs) {
  stopifnot(is.list(kwargs))
  if ("cat_vars" %in% names(kwargs)) {
    cat_vars <- kwargs[["cat_vars"]]
    params <- kwargs[names(kwargs) != "cat_vars"]
  } else {
    cat_vars <- NULL
    params <- kwargs
  }
  return(list(params = params, cat_vars = cat_vars))
}
