# Model training helper functions for the ML Visualizer app.

sigmoid_probability <- function(linear_values) {
  1 / (1 + exp(-linear_values))
}


train_logistic_regression_iterations <- function(classification_data,
                                                 prediction_grid,
                                                 prediction_threshold,
                                                 regularization_c = 1,
                                                 l1_ratio = 0,
                                                 fit_intercept = TRUE,
                                                 max_iter = 60,
                                                 tolerance = 0.001) {
  binary_target <- ifelse(classification_data$class == "Class B", 1, 0)

  # These fixed settings keep the training loop simple and readable
  # for beginners while still showing a visible learning process.
  learning_rate <- 0.12
  regularization_c <- max(regularization_c, 0.001)
  regularization_strength <- 1 / regularization_c
  l1_ratio <- min(max(l1_ratio, 0), 1)
  total_iterations <- max(1, as.integer(max_iter))
  tolerance <- max(tolerance, 0)

  weight_x <- 0
  weight_y <- 0
  bias <- 0
  previous_loss <- Inf
  actual_iteration_count <- 0

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

    l2_gradient_x <- (1 - l1_ratio) * regularization_strength * weight_x
    l2_gradient_y <- (1 - l1_ratio) * regularization_strength * weight_y

    l1_gradient_x <- l1_ratio * regularization_strength * sign(weight_x)
    l1_gradient_y <- l1_ratio * regularization_strength * sign(weight_y)

    gradient_weight_x <- gradient_weight_x + l2_gradient_x + l1_gradient_x
    gradient_weight_y <- gradient_weight_y + l2_gradient_y + l1_gradient_y

    weight_x <- weight_x - learning_rate * gradient_weight_x
    weight_y <- weight_y - learning_rate * gradient_weight_y
    if (fit_intercept) {
      bias <- bias - learning_rate * gradient_bias
    } else {
      bias <- 0
    }

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
    log_loss <- -mean(
      binary_target * log(safe_training_probabilities) +
        (1 - binary_target) * log(1 - safe_training_probabilities)
    )
    l2_penalty <- 0.5 * (1 - l1_ratio) * regularization_strength * (weight_x^2 + weight_y^2)
    l1_penalty <- l1_ratio * regularization_strength * (abs(weight_x) + abs(weight_y))
    current_loss <- log_loss + l2_penalty + l1_penalty

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

    actual_iteration_count <- iteration_index

    if (abs(previous_loss - current_loss) < tolerance) {
      break
    }

    previous_loss <- current_loss
  }

  iteration_history <- iteration_history[seq_len(actual_iteration_count)]
  loss_history <- loss_history[seq_len(actual_iteration_count)]
  accuracy_history <- accuracy_history[seq_len(actual_iteration_count)]

  final_iteration <- iteration_history[[actual_iteration_count]]

  list(
    model_object = list(
      weight_x = final_iteration$weight_x,
      weight_y = final_iteration$weight_y,
      bias = final_iteration$bias,
      learning_rate = learning_rate,
      total_iterations = actual_iteration_count,
      requested_max_iter = total_iterations,
      tolerance = tolerance,
      regularization_c = regularization_c,
      l1_ratio = l1_ratio,
      fit_intercept = fit_intercept
    ),
    prediction_grid = final_iteration$prediction_grid,
    training_predictions = final_iteration$training_predictions,
    metrics = final_iteration$metrics,
    iterations = iteration_history,
    iteration_metrics = data.frame(
      iteration = seq_len(actual_iteration_count),
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
    if (is.null(parameter_values)) {
      parameter_values <- list()
    }

    decision_threshold <- parameter_values$decision_threshold
    logistic_c <- parameter_values$logistic_c
    logistic_l1_ratio <- parameter_values$logistic_l1_ratio
    logistic_fit_intercept <- parameter_values$logistic_fit_intercept
    logistic_max_iter <- parameter_values$logistic_max_iter
    logistic_tol <- parameter_values$logistic_tol

    if (is.null(decision_threshold)) {
      decision_threshold <- 0.5
    }
    if (is.null(logistic_c)) {
      logistic_c <- 1
    }
    if (is.null(logistic_l1_ratio)) {
      logistic_l1_ratio <- 0
    }
    if (is.null(logistic_fit_intercept)) {
      logistic_fit_intercept <- TRUE
    }
    if (is.null(logistic_max_iter)) {
      logistic_max_iter <- 60
    }
    if (is.null(logistic_tol)) {
      logistic_tol <- 0.001
    }
    logistic_fit_intercept <- isTRUE(logistic_fit_intercept)

    if (!is.numeric(decision_threshold) || length(decision_threshold) != 1 || is.na(decision_threshold)) {
      stop("Decision threshold must be a single numeric value.")
    }

    if (decision_threshold < 0 || decision_threshold > 1) {
      stop("Decision threshold must be between 0 and 1.")
    }

    if (!is.numeric(logistic_c) || length(logistic_c) != 1 || is.na(logistic_c)) {
      stop("Regularization strength must be a single numeric value.")
    }

    if (logistic_c <= 0) {
      stop("Regularization strength must be greater than 0.")
    }

    if (!is.numeric(logistic_l1_ratio) || length(logistic_l1_ratio) != 1 || is.na(logistic_l1_ratio)) {
      stop("Regularization mix must be a single numeric value.")
    }

    if (logistic_l1_ratio < 0 || logistic_l1_ratio > 1) {
      stop("Regularization mix must be between 0 and 1.")
    }

    if (!is.numeric(logistic_max_iter) || length(logistic_max_iter) != 1 || is.na(logistic_max_iter)) {
      stop("Max iterations must be a single numeric value.")
    }

    if (logistic_max_iter < 1) {
      stop("Max iterations must be at least 1.")
    }

    if (!is.numeric(logistic_tol) || length(logistic_tol) != 1 || is.na(logistic_tol)) {
      stop("Stopping tolerance must be a single numeric value.")
    }

    if (logistic_tol < 0) {
      stop("Stopping tolerance must be 0 or greater.")
    }

    logistic_training_results <- train_logistic_regression_iterations(
      classification_data = classification_data,
      prediction_grid = prediction_grid,
      prediction_threshold = decision_threshold,
      regularization_c = logistic_c,
      l1_ratio = logistic_l1_ratio,
      fit_intercept = logistic_fit_intercept,
      max_iter = logistic_max_iter,
      tolerance = logistic_tol
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
