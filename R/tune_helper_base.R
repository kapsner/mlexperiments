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
    as.integer(k) >= 3L,
    is.integer(k <- as.integer(k))
  )

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
      if (nrow(self$parameter_grid) >
          as.integer(options("mlexperiments.bayesian.max_init"))) {
        message(sprintf(paste0(
          "\nNumber of rows of initialization grid > than ",
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
        names(self$parameter_bounds))) == 0L,
      no = TRUE
    ),
    length(intersect(
      names(private$method_helper$params_not_optimized),
      names(private$execute_params))) == 0L
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
    stopifnot(inherits(results_object, "bayesOpt"))
    outlist$bayesOpt <- results_object
    summary_object <- .bayesopt_postprocessing(
      self = self,
      private = private,
      object = results_object
    )

    param_names <- setdiff(
      colnames(summary_object),
      c("Epoch", "Iteration", "gpUtility",
        "acqOptimum", "inBounds", "Elapsed",
        "Score", "metric_optim_mean", "errorMessage"
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
  outlist[["summary"]] <- summary_object[, .SD, .SDcols = !exl_cols]

  outlist[["best.setting"]] <- .get_best_setting(
    results = outlist$summary,
    opt_metric = "metric_optim_mean",
    param_names = param_names,
    higher_better = metric_higher_better
  )
  # export also not optimized parameters (in case of bayesian) to best.setting
  outlist[["best.setting"]] <- kdry::list.append(
    outlist[["best.setting"]],
    private$method_helper$execute_params$params_not_optimized
  )
  outlist[["best.setting"]] <- outlist[["best.setting"]][
      !kdry::misc_duplicated_by_names(outlist[["best.setting"]])
    ]
  return(outlist)
}


.get_best_setting <- function(
    results,
    opt_metric,
    param_names,
    higher_better
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
  #%best_row <- results[FUN(get(opt_metric)), .SD, .SDcols = param_names]
  best_row <- res[best_row_id, which(colnames(res) %in% param_names)]
  stopifnot(nrow(best_row) == 1)
  ret <- as.list(best_row)
  return(ret[!kdry::misc_duplicated_by_names(ret, fromLast = TRUE)])
}
