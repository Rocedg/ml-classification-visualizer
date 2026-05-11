# Metrics helper functions for the ML Visualizer app.

# safe_metric_divide()
# Purpose:
#   Return a default value instead of crashing when a metric denominator is 0.
safe_metric_divide <- function(numerator, denominator, default = 0) {
  if (is.na(denominator) || denominator == 0) {
    return(default)
  }

  numerator / denominator
}


# calculate_classification_metrics()
# Purpose:
#   Compare true class labels with model predictions and summarize performance.
# Inputs:
#   - actual_labels: known Class A / Class B labels from the dataset
#   - predicted_labels: model output after applying the decision threshold
# Output:
#   Rounded accuracy, precision, recall, and F1 score values for the UI cards.
calculate_classification_metrics <- function(actual_labels, predicted_labels) {
  actual_labels <- factor(actual_labels, levels = c("Class A", "Class B"))
  predicted_labels <- factor(predicted_labels, levels = c("Class A", "Class B"))
  valid_rows <- !is.na(actual_labels) & !is.na(predicted_labels)
  actual_labels <- actual_labels[valid_rows]
  predicted_labels <- predicted_labels[valid_rows]

  if (length(actual_labels) == 0) {
    return(list(
      accuracy = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1_score = NA_real_
    ))
  }

  # Class B is treated as the positive class for precision, recall, and F1.
  true_positive <- sum(actual_labels == "Class B" & predicted_labels == "Class B")
  true_negative <- sum(actual_labels == "Class A" & predicted_labels == "Class A")
  false_positive <- sum(actual_labels == "Class A" & predicted_labels == "Class B")
  false_negative <- sum(actual_labels == "Class B" & predicted_labels == "Class A")

  accuracy <- (true_positive + true_negative) / length(actual_labels)
  precision <- safe_metric_divide(true_positive, true_positive + false_positive)
  recall <- safe_metric_divide(true_positive, true_positive + false_negative)
  f1_score <- safe_metric_divide(2 * precision * recall, precision + recall)

  list(
    accuracy = round(accuracy, 3),
    precision = round(precision, 3),
    recall = round(recall, 3),
    f1_score = round(f1_score, 3)
  )
}
