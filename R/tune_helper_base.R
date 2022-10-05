.run_tuning <- function(self, private, optimizer) {
  # run optimizer and return results
  .run_optimizer(self = self, private = private, optimizer = optimizer)
  return(self$results$summary)
}

.tune_init <- function(self, private, k) {
  stopifnot(
    !is.null(private$strategy),
    !is.null(self$learner),
    !is.null(private$x), !is.null(private$y),
    is.integer(as.integer(k))
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

  if (!is.null(private$cat_vars)) {
    private$method_helper$cat_vars <- private$cat_vars
  }

  if (!is.null(self$parameter_grid)) {
    # even if there is only one param setting, expand to grid here to make
    # this code working in any case
    if (!is.data.frame(self$parameter_grid)) {
      self$parameter_grid <- expand.grid(self$parameter_grid)
    }
    # to make use of the data.table-syntax, convert self$parameter_grid
    self$parameter_grid <- as.data.frame(
      self$parameter_grid,
      stringsAsFactors = FALSE
    )

    # check if there are additional parameters that are not tuned
    if (private$strategy == "bayesian") {
      if (nrow(self$parameter_grid) >
          as.integer(options("mlexperiments.bayesian.max_init"))) {
        message(sprintf(paste0(
          "Rows of initialization grid > than ",
          "'options(\"mlexperiments.bayesian.max_init\")'...\n",
          "... reducing initialization grid to %s rows."
        ), options("mlexperiments.bayesian.max_init")
        ))
        set.seed(private$seed)
        select_rows <- sample(
          x = seq_len(nrow(self$parameter_grid)),
          size = as.integer(options("mlexperiments.bayesian.max_init")),
          replace = FALSE
        )
        self$parameter_grid <- self$parameter_grid[select_rows, ]
      }
      if (length(colnames(self$parameter_grid)) !=
          length(names(self$parameter_bounds))) {
        vec <- which(
          colnames(self$parameter_grid) %in% names(self$parameter_bounds)
        )

        # if a column is an expression, data.table currently fails with an
        # error; data.frame is working, however, to select the appropriate
        # columns, we then convert them back to a data.table
        private$execute_params <- data.table::as.data.table(
          self$parameter_grid
        )[, .SD, .SDcols = vec]
        params_not_optimized <- data.table::as.data.table(
          self$parameter_grid[1, ]
        )[, .SD, .SDcols = !vec]
        stopifnot(nrow(params_not_optimized) == 1)
        private$method_helper$params_not_optimized <- params_not_optimized
      } else {
        private$execute_params <- self$parameter_grid
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
}

.optimize_postprocessing <- function(
    self,
    private,
    results_object,
    metric_higher_better
) {
  # define object to be returned
  outlist <- list()
  if (private$strategy == "bayesian") {
    stopifnot(inherits(results_object, "bayesOpt"))
    outlist$bayesOpt <- results_object
    outlist$summary <- .bayesopt_postprocessing(
      self = self,
      private = private,
      object = results_object
    )

    param_names <- setdiff(
      colnames(outlist$summary),
      c("Epoch", "Iteration", "gpUtility",
        "acqOptimum", "inBounds", "Elapsed",
        "Score", "metric_optim_mean", "errorMessage"
      )
    )
    opt_metric <- "Score"
  } else if (private$strategy == "grid") {
    stopifnot(inherits(results_object, "list"))
    outlist$summary <- data.table::rbindlist(
      l = results_object,
      fill = TRUE
    )

    param_names <- setdiff(
      colnames(outlist$summary),
      "metric_optim_mean"
    )
    opt_metric <- "metric_optim_mean"
  }

  outlist[["best.setting"]] <- .get_best_setting(
    results = outlist$summary,
    opt_metric = opt_metric,
    param_names = param_names,
    higher_better = metric_higher_better
  )
  # export also not optimized parameters (in case of bayesian) to best.setting
  if (!is.null(private$method_helper$params_not_optimized)) {
    outlist[["best.setting"]] <- c(
      outlist[["best.setting"]],
      private$method_helper$params_not_optimized
    )
  }
  return(outlist)
}


.get_best_setting <- function(
    results,
    opt_metric,
    param_names,
    higher_better = TRUE
) {
  stopifnot(
    data.table::is.data.table(results),
    is.character(opt_metric),
    is.logical(higher_better)
  )

  FUN <- ifelse(isTRUE(higher_better), which.max, which.min) # nolint
  # requires as data.table cannot handle expressions
  res <- as.data.frame(results)
  best_row_id <- FUN(res[, opt_metric])
  #best_row <- results[FUN(get(opt_metric)), .SD, .SDcols = param_names]
  best_row <- res[best_row_id, which(colnames(res) %in% param_names)]
  stopifnot(nrow(best_row) == 1)
  return(as.list(best_row))
}
