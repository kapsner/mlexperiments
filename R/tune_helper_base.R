.run_tuning <- function(self, private, optimizer) {
  # run optimizer and return results
  .run_optimizer(self = self, private = private, optimizer = optimizer)
  return(self$results$summary)
}

.tune_init <- function(self, private, k) {
  stopifnot(
    "`private$strategy` must not be `NULL`" = !is.null(private$strategy),
    "`private$learner` must not be `NULL`" = !is.null(self$learner),
    "`private$x` must not be `NULL`" = !is.null(private$x),
    "`private$y` must not be `NULL`" = !is.null(private$y),
    "`k` must be an integer >= 3L" = is.integer(as.integer(k)) &&
      as.integer(k) >= 3L
  )
  k <- as.integer(k)

  # add fold list, if it hasn't set manually
  if (is.null(private$method_helper$fold_list)) {
    if (is.null(self$split_vector)) {
      split_vector <- private$y
    } else {
      split_vector <- self$split_vector
    }
    stopifnot(
      is.atomic(split_vector),
      length(split_vector) == nrow(private$x)
    )
    private$method_helper$fold_list <- splitTools::create_folds(
      y = split_vector,
      k = k,
      type = self$split_type,
      m_rep = 1L,
      invert = FALSE,
      seed = private$seed
    )
  }

  if (!is.null(self$parameter_grid)) {
    # check if there are additional parameters that are not tuned
    if (private$strategy == "bayesian") {
      if (
        nrow(self$parameter_grid) >
          as.integer(options("mlexperiments.bayesian.max_init"))
      ) {
        message(sprintf(
          paste0(
            "\nNumber of rows of initialization grid > than ",
            "'options(\"mlexperiments.bayesian.max_init\")'...\n",
            "... reducing initialization grid to %s rows."
          ),
          options("mlexperiments.bayesian.max_init")
        ))
        set.seed(private$seed)
        select_rows <- sample(
          x = seq_len(nrow(self$parameter_grid)),
          size = as.integer(options("mlexperiments.bayesian.max_init")),
          replace = FALSE
        )
        self$parameter_grid <- kdry::mlh_subset(
          object = self$parameter_grid,
          ids = select_rows
        )
      }
    }
  } else {
    if (private$strategy == "grid") {
      stop(paste0(
        "Field 'parameter_grid' is empty - required for performing ",
        "a grid search"
      ))
    }
  }

  # apply parameter_grid stuff
  .organize_parameter_grid(self = self, private = private)

  stopifnot(
    ifelse(
      test = private$strategy == "bayesian",
      yes = length(intersect(
        names(private$method_helper$params_not_optimized),
        names(self$parameter_bounds)
      )) ==
        0L,
      no = TRUE
    ),
    length(intersect(
      names(private$method_helper$params_not_optimized),
      names(private$execute_params)
    )) ==
      0L
  )
}

.optimize_postprocessing <- function(
  self,
  private,
  results_object,
  metric_higher_better
) {
  stopifnot(is.logical(metric_higher_better))
  # define object to be returned
  outlist <- list()
  if (private$strategy == "bayesian") {
    stopifnot(inherits(results_object, "list"))
    summary_object <- .bayesopt_postprocessing(
      self = self,
      private = private,
      object = results_object
    )
    param_names <- setdiff(
      colnames(summary_object),
      c(
        "setting_id",
        "Value",
        "Round",
        "metric_optim_mean"
      )
    )
  } else if (private$strategy == "grid") {
    stopifnot(inherits(results_object, "list"))
    summary_object <- data.table::rbindlist(
      l = results_object,
      fill = TRUE
    )
    param_names <- setdiff(
      colnames(summary_object),
      "metric_optim_mean"
    )
  }

  exl_cols <- vapply(
    X = summary_object,
    FUN = is.expression,
    FUN.VALUE = logical(1L)
  )
  if ("cat_vars" %in% names(exl_cols)) {
    exl_cols["cat_vars"] <- TRUE
  }
  outlist[["summary"]] <- summary_object[, .SD, .SDcols = !exl_cols]


  best_row_id <- .get_best_setting_row_id(
    results = outlist$summary,
    opt_metric = "metric_optim_mean",
    higher_better = metric_higher_better
  )

  outlist[["best.setting"]] <- .get_best_setting(
    results = outlist$summary,
    best_row_id = best_row_id,
    param_names = param_names
  )

  if (private$strategy == "bayesian") {
    if (nrow(outlist$summary) != nrow(results_object$Pred) && nrow(results_object$Pred) == 1) {
      # assume, we have collected values there
      pred_cnames <- colnames(results_object$Pred)
      # get unique names
      pred_cn_unique <- gsub(
        pattern = "\\.\\d+$",
        replacement = "",
        x = pred_cnames
      ) |>
        unique()
      other_params <- setdiff(pred_cn_unique, "metric_optim_mean")
      if (length(other_params) > 0) {
        for (p in other_params) {
          c_index <- best_row_id - 1
          if (c_index > 0) {
            p_name <- paste0(p, ".", c_index)
          } else {
            p_name <- p
          }
          outlist[["best.setting"]][[p]] <- results_object$Pred[1, get(p_name)]
        }
      }
    }
    # delete Pred-col
    results_object$Pred <- NULL
    outlist[["bayesOpt"]] <- results_object
  }

  # export also not optimized parameters (in case of bayesian) to best.setting
  outlist[["best.setting"]] <- kdry::list.append(
    outlist[["best.setting"]],
    private$method_helper$execute_params$params_not_optimized
  )
  return(outlist)
}


.get_best_setting <- function(
  results,
  best_row_id,
  param_names
) {
  stopifnot(
    data.table::is.data.table(results)
  )
  #%best_row <- results[FUN(get(opt_metric)), .SD, .SDcols = param_names]
  show_cols <- intersect(param_names, colnames(results))
  stopifnot(length(show_cols) > 0)
  best_row <- results[best_row_id, .SD, .SDcols = show_cols]
  stopifnot(nrow(best_row) == 1)
  ret <- as.list(best_row)
  return(ret[!kdry::misc_duplicated_by_names(ret, fromLast = TRUE)])
}

.get_best_setting_row_id <- function(
  results,
  opt_metric,
  higher_better
) {
  stopifnot(
    data.table::is.data.table(results),
    is.character(opt_metric),
    is.logical(higher_better)
  )

  FUN <- ifelse(isTRUE(higher_better), which.max, which.min) # nolint
  # requires data.frame as data.table cannot handle expressions
  res <- as.data.frame(results)
  best_row_id <- FUN(res[, opt_metric])
  return(best_row_id)
}
