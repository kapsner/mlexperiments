.update_default_arguments <- function(new, default) {
  stopifnot(
    is.list(new), is.list(default),
    identical(intersect(names(new), names(default)), names(new))
  )

  updated <- sapply(
    X = names(default),
    FUN = function(x) {
      if (x %in% names(new)) {
        ifelse(
          test = new[[x]] != default[[x]],
          yes = new[[x]],
          no = default[[x]]
        )
      } else {
        default[[x]]
      }
    }
  )
  return(updated)
}

.argument_catcher <- function(...) {
  catch_args <- list(...)
  if ("..." %in% names(catch_args)) {
    catch_args <- catch_args[["..."]]
  }
  return(sapply(catch_args, list))
}
