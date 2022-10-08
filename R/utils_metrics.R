#' @export
metric <- function(name) {
  stopifnot(is.character(name) && length(name) == 1L)
  if (!requireNamespace("mlr3measures", quietly = TRUE)) {
    stop(
      paste0(
        "Package \"mlr3measures\" must be installed to use ",
        "function 'metric()'."
      ),
      call. = FALSE
    )
  }
  stopifnot(
    is.function(
      FUN <- utils::getFromNamespace(x = name, ns = "mlr3measures") # nolint
    )
  )

  fun_name <- paste0("mlr3measures::", name)

  default_args <- formals(FUN)
  response_name <- names(default_args)[2]
  stopifnot(response_name %in% c("response", "prob"))

  measure_metadata <- mlr3measures::measures[[name]]

  if (measure_metadata$type %in% c("classif", "binary")) {
    truth_factor <- TRUE
    if (measure_metadata$predict_type == "response") {
      predict_factor <- TRUE
    } else if (measure_metadata$predict_type == "prob") {
      predict_factor <- FALSE
    }
  } else {
    truth_factor <- FALSE
    predict_factor <- FALSE
  }

  fun_body_pre <- paste0(
    ifelse(
      test = isTRUE(truth_factor),
      yes = "\nground_truth <- factor(ground_truth)\n",
      no = "\nground_truth <- as.numeric(as.character(ground_truth))\n"
    ),
    ifelse(
      test = isTRUE(predict_factor),
      yes = "predictions <- factor(predictions)\n",
      no = "predictions <- as.numeric(as.character(predictions))\n"
    )
  )

  # compose function body
  fun_body <- paste0(
    "args <- list(truth = ground_truth, ", response_name, " = predictions) \n",
    "if (length(kwargs) > 0L) {\nargs <- c(args, kwargs)\n}\n",
    "return(do.call(", fun_name, ", args))"
  )

  fun <- paste0(
    "function(ground_truth, predictions, ...) {\n",
    "kwargs <- list(...)",
    fun_body_pre,
    fun_body,
    "\n}"
  )
  return(eval(parse(text = fun)))
}
