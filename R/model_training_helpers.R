# Model training helper functions for the ML Visualizer app.

sigmoid_probability <- function(linear_values) {
  1 / (1 + exp(-linear_values))
}


train_logistic_regression_iterations <- function(classification_data, prediction_grid, prediction_threshold) {
  binary_target <- ifelse(classification_data$class == "Class B", 1, 0)

  # These fixed settings keep the training loop simple and readable
  # for beginners while still showing a visible learning process.
  learning_rate <- 0.12
  total_iterations <- 60

  weight_x <- 0
  weight_y <- 0
  bias <- 0

  iteration_history <- vector("list", total_iterations)
  loss_history <- numeric(total_iterations)
  accuracy_history <- numeric(total_iterations)

  for (iteration_index in seq_len(total_iterations)) {
    # Step 1: use the current parameters to estimate Class B probabilities.
    linear_scores <- bias + weight_x * classification_data$x + weight_y * classification_data$y
    predicted_probabilities <- sigmoid_probability(linear_scores)

    # Step 2: measure how far the predictions are from the real labels.
    probability_error <- predicted_probabilities - binary_target

    # Step 3: compute one gradient descent update.
    gradient_weight_x <- mean(probability_error * classification_data$x)
    gradient_weight_y <- mean(probability_error * classification_data$y)
    gradient_bias <- mean(probability_error)

    weight_x <- weight_x - learning_rate * gradient_weight_x
    weight_y <- weight_y - learning_rate * gradient_weight_y
    bias <- bias - learning_rate * gradient_bias

    # Step 4: save the updated state so the UI can replay it later.
    updated_linear_scores <- bias + weight_x * classification_data$x + weight_y * classification_data$y
    updated_training_probabilities <- sigmoid_probability(updated_linear_scores)
    updated_grid_probabilities <- sigmoid_probability(
      bias + weight_x * prediction_grid$x + weight_y * prediction_grid$y
    )

    training_predictions <- ifelse(
      updated_training_probabilities >= prediction_threshold,
      "Class B",
      "Class A"
    )

    grid_predictions <- ifelse(
      updated_grid_probabilities >= prediction_threshold,
      "Class B",
      "Class A"
    )

    safe_training_probabilities <- pmin(pmax(updated_training_probabilities, 1e-6), 1 - 1e-6)
    current_loss <- -mean(
      binary_target * log(safe_training_probabilities) +
        (1 - binary_target) * log(1 - safe_training_probabilities)
    )

    training_predictions <- factor(training_predictions, levels = c("Class A", "Class B"))
    current_metrics <- calculate_classification_metrics(
      actual_labels = classification_data$class,
      predicted_labels = training_predictions
    )

    iteration_prediction_grid <- prediction_grid
    iteration_prediction_grid$predicted_class <- factor(
      grid_predictions,
      levels = c("Class A", "Class B")
    )
    iteration_prediction_grid$class_b_probability <- updated_grid_probabilities

    loss_history[iteration_index] <- current_loss
    accuracy_history[iteration_index] <- current_metrics$accuracy

    iteration_history[[iteration_index]] <- list(
      iteration_index = iteration_index,
      weight_x = weight_x,
      weight_y = weight_y,
      bias = bias,
      training_probabilities = updated_training_probabilities,
      training_predictions = training_predictions,
      prediction_grid = iteration_prediction_grid,
      loss_value = round(current_loss, 4),
      metrics = current_metrics
    )
  }

  final_iteration <- iteration_history[[total_iterations]]

  list(
    model_object = list(
      weight_x = final_iteration$weight_x,
      weight_y = final_iteration$weight_y,
      bias = final_iteration$bias,
      learning_rate = learning_rate,
      total_iterations = total_iterations
    ),
    prediction_grid = final_iteration$prediction_grid,
    training_predictions = final_iteration$training_predictions,
    metrics = final_iteration$metrics,
    iterations = iteration_history,
    iteration_metrics = data.frame(
      iteration = seq_len(total_iterations),
      loss = loss_history,
      accuracy = accuracy_history
    )
  )
}


train_classification_model <- function(classification_data, algorithm_name, parameter_values) {
  if (nrow(classification_data) < 6) {
    stop("Please provide at least 6 data points before training a classifier.")
  }

  if (length(unique(classification_data$class)) < 2) {
    stop("The dataset must contain both Class A and Class B.")
  }

  prediction_grid <- build_prediction_grid(classification_data)

  if (algorithm_name == "logistic_regression") {
    logistic_training_results <- train_logistic_regression_iterations(
      classification_data = classification_data,
      prediction_grid = prediction_grid,
      prediction_threshold = parameter_values$decision_threshold
    )

    algorithm_label <- "Logistic Regression"
  } else {
    stop("Only Logistic Regression is currently available for training.")
  }

  # SVM and k-NN training branches can be restored here when those
  # algorithms are ready to be enabled again in the UI.
  trained_model <- logistic_training_results$model_object
  prediction_grid <- logistic_training_results$prediction_grid
  training_predictions <- logistic_training_results$training_predictions
  metrics <- logistic_training_results$metrics
  iteration_history <- logistic_training_results$iterations
  iteration_metrics <- logistic_training_results$iteration_metrics

  list(
    algorithm_key = algorithm_name,
    algorithm_label = algorithm_label,
    model_object = trained_model,
    prediction_grid = prediction_grid,
    training_predictions = training_predictions,
    metrics = metrics,
    iterations = iteration_history,
    iteration_metrics = iteration_metrics
  )
}
