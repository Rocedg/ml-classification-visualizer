# Model dispatch helpers for the ML Visualizer app.
# train_classification_model()
# Purpose:
#   Validate user data, build the prediction grid, and dispatch training.
# Inputs:
#   - classification_data: current preset/uploaded/drawn dataset
#   - algorithm_name: selected algorithm key from the sidebar
#   - parameter_values: current algorithm controls from the sidebar
# Output:
#   A model result list consumed by plot, metrics, playback, and theory modules.
train_classification_model <- function(classification_data, algorithm_name, parameter_values) {
  if (is.null(parameter_values)) {
    parameter_values <- list()
  }

  if (nrow(classification_data) < 6) {
    stop("Please provide at least 6 data points before training a classifier.")
  }

  if (length(unique(classification_data$class)) < 2) {
    stop("The dataset must contain both Class A and Class B.")
  }

  split_classification_data <- create_train_test_split(classification_data)
  train_classification_data <- split_classification_data[split_classification_data$split == "train", , drop = FALSE]

  # The grid is created once for this run from all points. Every saved
  # iteration writes its probabilities into the same coordinate layout.
  prediction_grid <- build_prediction_grid(split_classification_data)

  if (algorithm_name == "logistic_regression") {
    if (nrow(train_classification_data) < 2) {
      stop("The training split needs at least 2 data points. Add more data before running the classifier.")
    }

    if (length(unique(train_classification_data$class)) < 2) {
      stop("The training split must contain both Class A and Class B. Add more balanced data before running the classifier.")
    }

    decision_threshold <- parameter_values$decision_threshold
    logistic_learning_rate <- parameter_values$logistic_learning_rate
    logistic_fit_intercept <- parameter_values$logistic_fit_intercept
    logistic_max_iter <- parameter_values$logistic_max_iter

    if (is.null(decision_threshold)) {
      decision_threshold <- 0.5
    }
    if (is.null(logistic_learning_rate)) {
      logistic_learning_rate <- 0.12
    }
    if (is.null(logistic_fit_intercept)) {
      logistic_fit_intercept <- TRUE
    }
    if (is.null(logistic_max_iter)) {
      logistic_max_iter <- 60
    }
    logistic_fit_intercept <- isTRUE(logistic_fit_intercept)

    # Parameter checks keep invalid UI or programmatic values from entering
    # the training loop.
    if (!is.numeric(decision_threshold) || length(decision_threshold) != 1 || is.na(decision_threshold)) {
      stop("Decision threshold must be a single numeric value.")
    }

    if (decision_threshold < 0 || decision_threshold > 1) {
      stop("Decision threshold must be between 0 and 1.")
    }

    if (!is.numeric(logistic_learning_rate) || length(logistic_learning_rate) != 1 || is.na(logistic_learning_rate)) {
      stop("Learning rate must be a single numeric value.")
    }

    if (logistic_learning_rate <= 0) {
      stop("Learning rate must be greater than 0.")
    }

    if (!is.numeric(logistic_max_iter) || length(logistic_max_iter) != 1 || is.na(logistic_max_iter)) {
      stop("Max iterations must be a single numeric value.")
    }

    if (logistic_max_iter < 1) {
      stop("Max iterations must be at least 1.")
    }

    logistic_training_results <- train_logistic_regression_iterations(
      classification_data = train_classification_data,
      prediction_grid = prediction_grid,
      prediction_threshold = decision_threshold,
      fit_intercept = logistic_fit_intercept,
      learning_rate = logistic_learning_rate,
      max_iter = logistic_max_iter,
      evaluation_data = split_classification_data
    )

    algorithm_label <- "Logistic Regression"
    algorithm_training_results <- logistic_training_results
  } else if (algorithm_name == "knn") {
    requested_k <- parameter_values$knn_k
    distance_metric <- normalize_knn_distance_metric(parameter_values$knn_distance_metric)
    voting_method <- normalize_knn_voting_method(parameter_values$knn_voting_method)

    if (is.null(requested_k)) {
      requested_k <- 5
    }

    if (!is.numeric(requested_k) || length(requested_k) != 1 || is.na(requested_k)) {
      stop("k neighbors must be a single numeric value.")
    }

    if (requested_k < 1) {
      stop("k neighbors must be at least 1.")
    }

    knn_training_results <- train_knn_classifier(
      classification_data = train_classification_data,
      prediction_grid = prediction_grid,
      k = requested_k,
      distance_metric = distance_metric,
      voting_method = voting_method,
      evaluation_data = split_classification_data
    )

    algorithm_label <- "k-NN"
    algorithm_training_results <- knn_training_results
  } else {
    stop("Only Logistic Regression and k-NN are currently available for training.")
  }

  trained_model <- algorithm_training_results$model_object
  prediction_grid <- algorithm_training_results$prediction_grid
  training_predictions <- algorithm_training_results$training_predictions
  metrics <- algorithm_training_results$metrics
  train_metrics <- algorithm_training_results$train_metrics
  test_metrics <- algorithm_training_results$test_metrics
  split_counts <- algorithm_training_results$split_counts
  iteration_history <- algorithm_training_results$iterations
  iteration_metrics <- algorithm_training_results$iteration_metrics

  list(
    algorithm_key = algorithm_name,
    algorithm_label = algorithm_label,
    model_object = trained_model,
    classification_data = algorithm_training_results$classification_data,
    train_data = algorithm_training_results$train_data,
    test_data = algorithm_training_results$test_data,
    split_counts = split_counts,
    prediction_grid = prediction_grid,
    training_predictions = training_predictions,
    metrics = metrics,
    train_metrics = train_metrics,
    test_metrics = test_metrics,
    iterations = iteration_history,
    iteration_metrics = iteration_metrics
  )
}
