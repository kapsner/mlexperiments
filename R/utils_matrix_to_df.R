# converts a matrix to a data.table. This is important, e.g. for ranger and
# coxph in order to restore R-data types such as factors.
.matrix_to_df <- function(matrix, cat_vars = NULL) {
  if (inherits(x = matrix, what = c("matrix", "array"))) {
    matrix_dt <- data.table::as.data.table(matrix)
  } else if (inherits(x = matrix, what = c("data.frame"))) {
    matrix_dt <- data.table::as.data.table(data.matrix(matrix))
  }

  if (!is.null(cat_vars)) {
    cols_to_factor <- intersect(colnames(matrix_dt), cat_vars)
    matrix_dt[
      ,
      (cols_to_factor) := lapply(X = .SD, FUN = factor),
      .SDcols = cols_to_factor
    ]
  }
  return(matrix_dt)
}
