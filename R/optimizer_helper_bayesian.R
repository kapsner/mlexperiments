.bayesian_optimize <- function(
  self,
  private,
  x,
  y,
  method_helper
) {
  stopifnot(
    "`parameter_bounds` must not be empty for Bayesian optimization" = !is.null(
      self$parameter_bounds
    ),
    "`ncores` must be >1L when using Bayesian optimization" = private$ncores >
      1L
  )
  if (self$optim_args$parallel) {
    self$optim_args$parallel <- NULL # (specific for rBayesianOptimization)
  }
  # to not create issue afterwards, when args are initialized (specific for rBayesianOptimization)
  self$optim_args$init_grid_dt <- NULL
  # cluster options
  cluster_options <- kdry::misc_subset_options("mlexperiments")
  # required for cluster export
  assign(
    x = "seed",
    value = private$seed
  )

  env_args <- list(
    "x" = x,
    "y" = y,
    "seed" = seed,
    "method_helper" = method_helper, # , "ncores" #, "cluster_load"
    "cluster_options" = cluster_options
  )

  # export from global env
  # if (private$method %in% options("mlexperiments.learner")) {
  if (self$learner$environment != -1L) {
    # https://stackoverflow.com/questions/67595111/r-package-design-how-to-
    # export-internal-functions-to-a-cluster
    #%ns <- asNamespace("mlexperiments")
    stopifnot(
      "`learner$environment` must be a character" = is.character(
        self$learner$environment
      )
    )
    use_env <- asNamespace(self$learner$environment)
  } else {
    use_env <- -1L
  }

  get_from_env <- as.list(as.environment(use_env))
  get_from_env <- sapply(
    X = self$learner$cluster_export,
    FUN = function(x) {
      get_from_env[[x]]
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )

  env_args <- kdry::list.append(
    main_list = env_args,
    append_list = get_from_env
  )

  args <- kdry::list.append(
    list(
      # for each method, a bayesian scoring function is required
      # FUN = eval(parse(text = paste0(
      #   private$method, "_bsF"
      # ))),
      FUN = NULL,
      bounds = self$parameter_bounds,
      init_grid_dt = method_helper$execute_params$parameter_grid,
      env_args = env_args
    ),
    self$optim_args
  )

  # avoid error when setting initGrid / or initPoints
  if (!is.null(method_helper$execute_params$parameter_grid)) {
    args <- args[names(args) != "init_points"]
  } else {
    args <- args[names(args) != "init_grid_dt"]
  }

  set.seed(private$seed)
  opt_obj <- do.call(
    what = self$learner$bayesian_scoring_function,
    args = args,
  )
  return(opt_obj)
}

.bayesopt_postprocessing <- function(self, private, object) {
  stopifnot(
    "`object` is not of class `bayesOpt`" = inherits(
      x = object,
      what = "bayesOpt"
    )
  )
  exl_cols <- vapply(
    X = private$method_helper$execute_params$params_not_optimized,
    FUN = is.expression,
    FUN.VALUE = logical(1L)
  )

  # remove case_weights from params, otherwise displaying is very strange
  if ("case_weights" %in% names(self$learner_args)) {
    exl_cols["case_weights"] <- TRUE
  }
  optim_results <- cbind(
    data.table::as.data.table(object$scoreSummary),
    data.table::as.data.table(
      private$method_helper$execute_params$params_not_optimized[!exl_cols]
    )
  )

  colnames(optim_results)[grepl(
    pattern = "Iteration",
    x = colnames(optim_results)
  )] <- "setting_id"

  return(optim_results)
}
