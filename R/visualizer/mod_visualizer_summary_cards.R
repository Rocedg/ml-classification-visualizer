# R/visualizer/mod_visualizer_summary_cards.R
# Purpose:
#   Build the right-side summary and metrics cards for the plot tab.

visualizer_summary_cards_ui <- function(ns, help_icon) {
  div(
    class = "plot-side-panel",
    div(
      class = "app-card current-run-card",
      div(
        class = "current-run-header",
        tags$span("Current run"),
        help_icon("Summary of the dataset, model, and parameters used in the current visualization.")
      ),
      uiOutput(ns("current_run_summary"))
    ),
    div(
      class = "app-card metrics-comparison-card",
      div(
        class = "metrics-comparison-header",
        tags$span("Model performance"),
        help_icon("Training points fit the model. Test points are held out and used only to evaluate it.")
      ),
      uiOutput(ns("metrics_summary_ui"))
    )
  )
}


format_current_run_number <- function(value, digits = 2) {
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return("â€”")
  }

  numeric_value <- suppressWarnings(as.numeric(value))

  if (is.na(numeric_value)) {
    return("â€”")
  }

  formatC(numeric_value, format = "f", digits = digits)
}


format_metric_value <- function(metrics, metric_name) {
  if (is.null(metrics) || is.null(metrics[[metric_name]])) {
    return("â€”")
  }

  metric_value <- suppressWarnings(as.numeric(metrics[[metric_name]]))

  if (length(metric_value) != 1 || is.na(metric_value) || !is.finite(metric_value)) {
    return("â€”")
  }

  formatC(metric_value, format = "f", digits = 3)
}


format_current_run_text <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value) || !nzchar(as.character(value))) {
    return("â€”")
  }

  as.character(value)
}


format_algorithm_label <- function(algorithm_key) {
  if (is.null(algorithm_key) || length(algorithm_key) != 1 || is.na(algorithm_key)) {
    return("â€”")
  }

  switch(
    as.character(algorithm_key),
    logistic_regression = "Logistic Regression",
    svm = "SVM",
    knn = "k-NN",
    as.character(algorithm_key)
  )
}


format_intercept_label <- function(fit_intercept) {
  if (is.null(fit_intercept) || length(fit_intercept) != 1 || is.na(fit_intercept)) {
    return("â€”")
  }

  if (isTRUE(fit_intercept)) "ON" else "OFF"
}


format_knn_distance_label <- function(distance_metric) {
  switch(
    normalize_knn_distance_metric(distance_metric),
    euclidean = "Euclidean",
    manhattan = "Manhattan",
    "Euclidean"
  )
}


format_knn_voting_label <- function(voting_method) {
  switch(
    normalize_knn_voting_method(voting_method),
    uniform = "Uniform",
    distance_weighted = "Distance-weighted",
    "Uniform"
  )
}


format_current_run_integer <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value)) {
    return("â€”")
  }

  numeric_value <- suppressWarnings(as.numeric(value))

  if (is.na(numeric_value) || !is.finite(numeric_value)) {
    return("â€”")
  }

  as.character(as.integer(round(numeric_value)))
}


format_split_summary <- function(model_results) {
  if (is.null(model_results) || is.null(model_results$split_counts)) {
    return("70 / 30")
  }

  train_count <- model_results$split_counts$train
  test_count <- model_results$split_counts$test

  if (is.null(train_count) || is.null(test_count)) {
    return("70 / 30")
  }

  paste0(train_count, " / ", test_count)
}


visualizer_current_run_summary_ui <- function(parameter_values,
                                              model_results,
                                              selected_dataset_label,
                                              algorithm_key,
                                              iteration_text) {
  summary_row <- function(label_text, value_text) {
    div(
      class = "current-run-row",
      tags$span(class = "current-run-label", label_text),
      tags$span(class = "current-run-value", value_text)
    )
  }

  if (identical(algorithm_key, "knn")) {
    knn_distance_metric <- parameter_values$knn_distance_metric
    knn_voting_method <- parameter_values$knn_voting_method

    if (is.null(knn_distance_metric) && !is.null(model_results$model_object$distance_metric)) {
      knn_distance_metric <- model_results$model_object$distance_metric
    }
    if (is.null(knn_voting_method) && !is.null(model_results$model_object$voting_method)) {
      knn_voting_method <- model_results$model_object$voting_method
    }

    return(tagList(
      summary_row("Dataset", format_current_run_text(selected_dataset_label)),
      summary_row("Model", format_algorithm_label(algorithm_key)),
      summary_row("Split 70/30", format_split_summary(model_results)),
      summary_row("K neighbors", format_current_run_integer(parameter_values$knn_k)),
      summary_row("Distance", format_knn_distance_label(knn_distance_metric)),
      summary_row("Voting", format_knn_voting_label(knn_voting_method))
    ))
  }

  if (identical(algorithm_key, "logistic_regression")) {
    return(tagList(
      summary_row("Dataset", format_current_run_text(selected_dataset_label)),
      summary_row("Model", format_algorithm_label(algorithm_key)),
      summary_row("Iteration", iteration_text),
      summary_row("Split 70/30", format_split_summary(model_results)),
      summary_row("Learning rate", format_current_run_number(parameter_values$logistic_learning_rate)),
      summary_row("Threshold", format_current_run_number(parameter_values$decision_threshold)),
      summary_row("Intercept", format_intercept_label(parameter_values$logistic_fit_intercept))
    ))
  }

  tagList(
    summary_row("Dataset", format_current_run_text(selected_dataset_label)),
    summary_row("Model", format_algorithm_label(algorithm_key)),
    summary_row("Split 70/30", format_split_summary(model_results))
  )
}


visualizer_metrics_summary_ui <- function(train_metrics, test_metrics) {
  metric_row <- function(label_text, metric_name) {
    div(
      class = "metrics-comparison-row",
      tags$span(class = "metrics-comparison-label", label_text),
      tags$span(class = "metrics-comparison-value", format_metric_value(train_metrics, metric_name)),
      tags$span(class = "metrics-comparison-value metrics-comparison-test-value", format_metric_value(test_metrics, metric_name))
    )
  }

  tagList(
    div(
      class = "metrics-comparison-row metrics-comparison-row-heading",
      tags$span("Metric"),
      tags$span("Train"),
      tags$span(class = "metrics-comparison-test-heading", "Test")
    ),
    metric_row("Accuracy", "accuracy"),
    metric_row("Precision", "precision"),
    metric_row("Recall", "recall"),
    metric_row("F1", "f1_score"),
    tags$p(
      class = "metrics-comparison-note",
      "A large train-test gap can indicate overfitting."
    )
  )
}
