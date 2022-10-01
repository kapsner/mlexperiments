.bayesian_optimize <- function(
    self, private,
    x,
    y,
    seed,
    method_helper,
    ncores
) {
  stopifnot(!is.null(self$parameter_bounds))
  if (self$optim_args$parallel) {
    stopifnot(!is.null(private$learner$cluster_export))
    message(sprintf("Registering parallel backend using %s cores.", ncores))

    cl <- register_parallel(ncores)

    self$optim_args$iters.k <- ncores

    on.exit(
      expr = {
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
        # reset random number generator
        RNGkind(kind = "default")
        invisible(gc())
      }
    )
    # export from current env
    parallel::clusterExport(
      cl = cl,
      varlist = c(
        "x", "y", "seed", "method_helper", "ncores" #, "cluster_load"
      ),
      envir = environment()
    )

    # export from global env
    # if (private$method %in% options("mlexperiments.learner")) {
    if (private$learner$environment != -1L) {
      # https://stackoverflow.com/questions/67595111/r-package-design-how-to-
      # export-internal-functions-to-a-cluster
      #ns <- asNamespace("mlexperiments")
      stopifnot(is.character(private$learner$environment))
      ns <- asNamespace(private$learner$environment)
      parallel::clusterExport(
        cl = cl,
        # varlist = unclass(
        #   utils::lsf.str(
        #     envir = ns,
        #     all = TRUE
        # )),
        varlist = private$learner$cluster_export,
        envir = as.environment(ns)
      )
    } else {
      parallel::clusterExport(
        cl = cl,
        varlist = private$learner$cluster_export,
        envir = -1L
      )
    }
    parallel::clusterSetRNGStream(
      cl = cl,
      iseed = seed
    )
    parallel::clusterEvalQ(
      cl = cl,
      expr = {
        #lapply(cluster_load, library, character.only = TRUE)
        ## not necessary since using ::-notation everywhere
        RNGkind("L'Ecuyer-CMRG")
        # set seed in each job for reproducibility
        set.seed(seed) #, kind = "L'Ecuyer-CMRG")
      }
    )
  }

  # in any case, update gsPoints here, as default calculation fails when
  # calling bayesOpt with do.call
  if (identical(str2lang("pmax(100, length(bounds)^3)"),
                self$optim_args[["gsPoints"]])) {
    self$optim_args[["gsPoints"]] <- pmax(100, length(self$parameter_bounds)^3)
  }

  args <- c(
    list(
      # for each method, a bayesian scoring function is required
      # FUN = eval(parse(text = paste0(
      #   private$method, "_bsF"
      # ))),
      FUN = private$learner$bayesian_scoring_function,
      bounds = self$parameter_bounds,
      initGrid = self$parameter_grid
    ),
    self$optim_args
  )

  # avoid error when setting initGrid / or initPoints
  if (!is.null(self$parameter_grid)) {
    args <- args[names(args) != "initPoints"]
  } else {
    args <- args[names(args) != "initGrid"]
  }

  set.seed(seed)
  opt_obj <- do.call(ParBayesianOptimization::bayesOpt, args)
  return(opt_obj)
}

.bayesopt_postprocessing <- function(self, private, object) {
  stopifnot(inherits(x = object, what = "bayesOpt"))
  optim_results <- object$scoreSummary %>%
    data.table::as.data.table() %>%
    cbind(private$params_not_optimized)

  colnames(optim_results)[grepl(
    pattern = "Iteration", x = colnames(optim_results))
  ] <- "setting_id"

  return(optim_results)
}
