#' @export
MLTuneParameters <- R6::R6Class( # nolint
  classname = "MLTuneParameters",
  inherit = MLExperimentsBase,
  public = list(
    parameter_bounds = NULL,
    parameter_grid = NULL,
    optim_args = NULL,
    #' @field split_type A character. The splitting strategy passed further on
    #'   to `splitTools` (Default: `stratified`).
    split_type = NULL,
    split_vector = NULL,
    initialize = function(
      learner,
      seed,
      strategy = c("grid", "bayesian"),
      ncores = -1L
    ) {
      super$initialize(learner = learner, seed = seed, ncores = ncores)
      stopifnot(
        !is.null(self$learner$.__enclos_env__$private$fun_optim_cv)
      )
      strategy <- match.arg(strategy)
      stopifnot(
        ifelse(
          test = strategy == "bayesian",
          yes = !is.null(self$learner$cluster_export),
          no = TRUE
        ),
        !is.null(
          self$learner$.__enclos_env__$private$fun_bayesian_scoring_function
        )
      )
      private$strategy <- strategy

      # init some stuff
      private$method_helper <- list()
      self$split_type <- "stratified"

      private$select_optimizer <- switch(
        EXPR = strategy,
        "grid" = .config_grid_optimizer,
        "bayesian" = .config_bayesian_optimizer
      )

    },
    #' @param k An integer to define the number of cross-validation folds used
    #'   to tune the hyperparameters.
    execute = function(k) {
      .tune_init(self, private, k)
      optimizer <- private$select_optimizer(self, private)
      return(.run_tuning(self = self, private = private, optimizer = optimizer))
    }
  ),
  private = list(
    select_optimizer = NULL,
    strategy = NULL,
    tune_params = NULL
  )
)
