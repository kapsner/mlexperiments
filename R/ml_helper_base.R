.organize_parameter_grid <- function(self, private) {
  if (!is.null(private$cat_vars)) {
    private$method_helper$execute_params$cat_vars <- private$cat_vars
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

    # logic to detect zero-variance variables
    zero_variance <- vapply(
      X = self$parameter_grid,
      FUN = function(x) {
        if (is.expression(x)) {
          # all expressions are considered to be zero-variance in grid
          return(TRUE)
        } else {
          if (length(unique(x)) == 1L) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      },
      FUN.VALUE = logical(1L)
    )
    vec <- colnames(self$parameter_grid)[zero_variance]

    if (length(vec) > 0L) {
      # if a column is an expression, data.table currently fails with an
      # error; data.frame is working, however, to select the appropriate
      # columns, we then convert them back to a data.table
      private$method_helper$execute_params$parameter_grid <-
        data.table::as.data.table(
        self$parameter_grid
      )[, .SD, .SDcols = !vec]
      params_not_optimized <- data.table::as.data.table(
        self$parameter_grid[1L, ]
      )[, .SD, .SDcols = vec]
      # sapply trick to remove attributes
      private$method_helper$execute_params$params_not_optimized <- sapply(
        X = names(params_not_optimized),
        FUN = function(x) {
          params_not_optimized[[x]]
        },
        simplify = FALSE,
        USE.NAMES = TRUE
      )
    } else {
      private$method_helper$execute_params$parameter_grid <-
        data.table::as.data.table(self$parameter_grid)
    }
  }

  # append learner_args to params_not_optimized
  if (!is.null(self$learner_args)) {
    stopifnot(
      is.list(self$learner_args),
      ifelse(
        test = !is.null(
          private$method_helper$execute_params$params_not_optimized
        ),
        yes = length(setdiff(#
          names(self$learner_args),
          names(
            private$method_helper$execute_params$params_not_optimized
          ))) == length(self$learner_args),
        no = TRUE
      ),
      length(setdiff(names(self$learner_args),
              names(private$method_helper$execute_params$parameter_grid))) ==
        length(self$learner_args)
    )


    private$method_helper$execute_params$params_not_optimized <- kdry::list.append(
      self$learner_args,
      private$method_helper$execute_params$params_not_optimized
    )
  }
}
