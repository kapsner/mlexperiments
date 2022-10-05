.grid_optimize <- function(
    self, private,
    x,
    y,
    method_helper
) {
  stopifnot(
    (ngrid <- nrow(self$parameter_grid)) > 1L
  )

  # init a progress bar
  pb <- progress::progress_bar$new(
    format = "Parameter settings [:bar] :current/:total (:percent)",
    total = ngrid
  )

  optim_results <- lapply(
    X = seq_len(ngrid),
    FUN = function(setting_id) {

      # increment progress bar
      pb$tick()

      # get the relevant row from param_list with the hyperparameters to use in
      # this loop
      # this code is required to have names arguments and allow selection of
      # expressions (which is not possible with data.table)
      grid_search_params <- sapply(
        X = colnames(self$parameter_grid),
        FUN = function(x) {
          xcol <- which(colnames(self$parameter_grid) == x)
          self$parameter_grid[setting_id, xcol]
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )

      # FUN <- eval(parse(text = paste0(
      #   private$method, "_cv"
      # )))
      FUN <- private$learner$cross_validation # nolint

      fun_parameters <- list(
        "x" = x,
        "y" = y,
        "params" = grid_search_params,
        "fold_list" = method_helper$fold_list,
        "ncores" = private$ncores,
        "seed" = private$seed
      )

      set.seed(private$seed)
      fit_grid <- do.call(FUN, fun_parameters)

      ret <- data.table::as.data.table(
        c(
          list("setting_id" = setting_id),
          fit_grid,
          grid_search_params[
            setdiff(names(grid_search_params), names(fit_grid))
          ]
        )
      )
      #return(ret[, .SD, .SDcols = colnames(ret)[!sapply(ret, is.expression)]])
      return(ret)
    }
  )
  return(optim_results)
}
