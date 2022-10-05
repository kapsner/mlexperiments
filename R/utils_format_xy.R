.format_xy <- function(object, ids) {
  if (is.null(dim(object)) && is.atomic(object)) {
    return(object[ids])
  } else {
    return(object[ids, ])
  }
}
