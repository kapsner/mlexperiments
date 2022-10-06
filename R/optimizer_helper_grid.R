.grid_optimize <- function(
    self, private,
    x,
    y,
    method_helper
) {
  stopifnot(
    (ngrid <- nrow(private$execute_params)) > 1L
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
        X = colnames(private$execute_params),
        FUN = function(x) {
          xcol <- which(colnames(private$execute_params) == x)
          private$execute_params[setting_id, xcol]
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )

      params <- .method_params_refactor(
        grid_search_params,
        method_helper
      )

      # FUN <- eval(parse(text = paste0(
      #   private$method, "_cv"
      # )))
      FUN <- private$learner$cross_validation # nolint

      fun_parameters <- list(
        "x" = x,
        "y" = y,
        "params" = params,
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
          params[
            setdiff(names(params), names(fit_grid))
          ]
        )
      )
      #%return(ret[, .SD, .SDcols = colnames(ret)[!sapply(ret, is.expression)]])
      return(ret)
    }
  )
  return(optim_results)
}
