#' @export
validate_fold_equality <- function(experiments) {
  stopifnot(
    is.list(experiments) || length(experiments) > 1L,
    all(sapply(
      X = experiments,
      FUN = function(x) {
        inherits(x, "MLCrossValidation")
      }
    )) || all(sapply(
      X = experiments,
      FUN = function(x) {
        inherits(x, "MLTuneParameters")
      }
    ))
  )

  if (inherits(experiments[[1]], "MLCrossValidation")) {
    fold_lists <- "experiments[[%s]]$fold_list"
  } else if (inherits(experiments[[1]], "MLTuneParameters")) {
    fold_lists <- "experiments[[%s]]$results$fold_list"
  }

  for (i in seq_len(length(experiments))) {
    if (i < length(experiments)) {
      test_i <- i + 1L
    } else {
      test_i <- 1L
    }
    message(sprintf("\nTesting for identical folds in %s and %s.", i, test_i))
    stopifnot(
      all(
        sapply(
          X = names(eval(parse(text = sprintf(fold_lists, "i")))),
          FUN = function(x) {
            all(eval(parse(text = sprintf(fold_lists, "i")))[[x]] ==
                  eval(parse(text = sprintf(fold_lists, "test_i")))[[x]])
          }
        )
      )
    )
  }
}
