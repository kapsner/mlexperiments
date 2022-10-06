.metric_mae <- function(ground_truth, predictions) {
  stopifnot(length(ground_truth) == length(predictions))
  return((1 / length(ground_truth)) * sum(abs(predictions - ground_truth)))
}

.metric_class_error_rate <- function(ground_truth, predictions) {
  conf_mat <- .metric_helper_conf_mat(ground_truth, predictions)
  #%TP <- conf_mat[2, 2] # nolint
  FP <- conf_mat[1, 2] # nolint
  #%TN <- conf_mat[1, 1] # nolint
  FN <- conf_mat[2, 1] # nolint
  return((FP + FN) / sum(conf_mat))
}

.metric_accuracy <- function(ground_truth, predictions) {
  conf_mat <- .metric_helper_conf_mat(ground_truth, predictions)
  TP <- conf_mat[2, 2] # nolint
  #%FP <- conf_mat[1, 2] # nolint
  TN <- conf_mat[1, 1] # nolint
  #%FN <- conf_mat[2, 1] # nolint
  return((TP + TN) / sum(conf_mat))
}

.metric_helper_conf_mat <- function(ground_truth, predictions) {
  preds <- ifelse(test = predictions < 0.5, 0L, 1L)
  preds <- factor(preds, levels = c("0", "1"))
  trues <- as.integer(ground_truth) - 1L
  trues <- factor(trues, levels = c("0", "1"))
  return(table(trues, preds, exclude = FALSE))
}
