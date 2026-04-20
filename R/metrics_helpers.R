# Metrics helper functions for the ML Visualizer app.

calculate_classification_metrics <- function(actual_labels, predicted_labels) {
  actual_labels <- factor(actual_labels, levels = c("Class A", "Class B"))
  predicted_labels <- factor(predicted_labels, levels = c("Class A", "Class B"))

  true_positive <- sum(actual_labels == "Class B" & predicted_labels == "Class B")
  true_negative <- sum(actual_labels == "Class A" & predicted_labels == "Class A")
  false_positive <- sum(actual_labels == "Class A" & predicted_labels == "Class B")
  false_negative <- sum(actual_labels == "Class B" & predicted_labels == "Class A")

  accuracy <- (true_positive + true_negative) / length(actual_labels)
  precision <- if ((true_positive + false_positive) == 0) 0 else true_positive / (true_positive + false_positive)
  recall <- if ((true_positive + false_negative) == 0) 0 else true_positive / (true_positive + false_negative)
  f1_score <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)

  list(
    accuracy = round(accuracy, 3),
    precision = round(precision, 3),
    recall = round(recall, 3),
    f1_score = round(f1_score, 3)
  )
}
