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
      grid_search_params <- as.list(self$parameter_grid[setting_id, ])

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
      return(ret)
    }
  )
  return(optim_results)
}
