.outsample_row_indices <- function(
    fold_list,
    training_data_nrows,
    type = NULL
  ) {
  fold_ids <- sapply(
    X = fold_list,
    FUN = function(x) {
      setdiff(1:training_data_nrows, x)
    }
  )

  if (is.null(type)) {
    return(fold_list)
  } else if (type == "glmnet") {
    # assign each row of the dataset to a specific test fold
    fids <- data.table::data.table()

    for (fid in seq_along(fold_ids)) {
      fids <- data.table::rbindlist(
        l = list(
          fids,
          data.table::data.table(
            "row_id" = fold_ids[[fid]],
            "fold_id" = fid
          )
        )
      )
    }
    fids <- fids[order(get("row_id"))]
    stopifnot(
      !any(duplicated(fids$row_id)),
      length(unique(fids$fold_id)) == length(fold_list)
    )
    return(fids)
  } else {
    stop(paste0("Type '", type, "' not implemented."))
  }
}
