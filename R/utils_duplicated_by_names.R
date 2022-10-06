duplicated_by_names <- function(object, ...) {
  stopifnot(names(object) > 0L)
  return(duplicated(x = names(object), ...))
}
