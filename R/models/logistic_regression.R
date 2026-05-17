# Logistic Regression backend helpers for the ML Visualizer app.
# sigmoid_probability()
# Purpose:
#   Convert linear model scores into probabilities between 0 and 1.
# Input:
#   - linear_values: bias + weight_x * x + weight_y * y scores
# Output:
#   Probability values interpreted here as Class B probability.
sigmoid_probability <- function(linear_values) {
  1 / (1 + exp(-linear_values))
}


# train_logistic_regression_iterations()
# Purpose:
#   Train a simple logistic regression model while saving every iteration.
# Inputs:
#   - classification_data: train split rows used for gradient descent
#   - evaluation_data: full run data, including train/test split labels
#   - prediction_grid: x/y locations used to draw the probability heatmap
#   - prediction_threshold: probability cutoff for Class B predictions
#   - fit_intercept: whether the bias/intercept term is learned
#   - learning_rate, max_iter: gradient descent controls
# Output:
#   A model bundle containing final parameters, metrics, prediction grid,
#   iteration history, and per-iteration metric data for visualization.
train_logistic_regression_iterations <- function(classification_data,
                                                 prediction_grid,
                                                 prediction_threshold,
                                                 fit_intercept = TRUE,
                                                 learning_rate = 0.12,
                                                 max_iter = 60,
                                                 evaluation_data = classification_data) {
  # The training math uses 1 for Class B and 0 for Class A.
  binary_target <- ifelse(classification_data$class == "Class B", 1, 0)

  # These settings keep the training loop simple and readable for beginners
  # while still showing a visible learning process.
  learning_rate <- max(learning_rate, 0.001)
  total_iterations <- max(1, as.integer(max_iter))

  if (!"split" %in% names(evaluation_data)) {
    evaluation_data$split <- factor(rep("train", nrow(evaluation_data)), levels = c("train", "test"))
  } else {
    evaluation_data$split <- factor(as.character(evaluation_data$split), levels = c("train", "test"))
  }

  # weight_x and weight_y control the tilt of the decision surface.
  # bias is the intercept term, which shifts the boundary when it is learned.
  weight_x <- 0
  weight_y <- 0
  bias <- 0
  actual_iteration_count <- 0
  saved_iteration_count <- 0

  iteration_history <- vector("list", total_iterations + 1)
  loss_history <- numeric(total_iterations + 1)
  accuracy_history <- numeric(total_iterations + 1)

  # build_saved_iteration() packages one model state for playback.
  # It evaluates train/test points and the full prediction grid so the UI can
  # redraw the heatmap and metrics at any saved iteration.
  build_saved_iteration <- function(iteration_index, current_weight_x, current_weight_y, current_bias) {
    # A logistic model first creates a linear score, then passes it through
    # the sigmoid curve to get a Class B probability.
    current_training_probabilities <- sigmoid_probability(
      current_bias + current_weight_x * classification_data$x + current_weight_y * classification_data$y
    )
    current_grid_probabilities <- sigmoid_probability(
      current_bias + current_weight_x * prediction_grid$x + current_weight_y * prediction_grid$y
    )
    current_evaluation_probabilities <- sigmoid_probability(
      current_bias + current_weight_x * evaluation_data$x + current_weight_y * evaluation_data$y
    )

    training_predictions <- ifelse(
      current_training_probabilities >= prediction_threshold,
      "Class B",
      "Class A"
    )

    grid_predictions <- ifelse(
      current_grid_probabilities >= prediction_threshold,
      "Class B",
      "Class A"
    )
    evaluation_predictions <- ifelse(
      current_evaluation_probabilities >= prediction_threshold,
      "Class B",
      "Class A"
    )

    # Probabilities are clamped only for the log calculation so log(0) never
    # occurs; the unclamped probabilities remain available for visualization.
    safe_training_probabilities <- pmin(pmax(current_training_probabilities, 1e-6), 1 - 1e-6)
    log_loss <- -mean(
      binary_target * log(safe_training_probabilities) +
        (1 - binary_target) * log(1 - safe_training_probabilities)
    )
    current_loss <- log_loss

    training_predictions <- factor(training_predictions, levels = c("Class A", "Class B"))
    current_metrics <- calculate_classification_metrics(
      actual_labels = classification_data$class,
      predicted_labels = training_predictions
    )

    iteration_evaluation_data <- evaluation_data
    iteration_evaluation_data$predicted_class <- factor(
      evaluation_predictions,
      levels = c("Class A", "Class B")
    )
    iteration_evaluation_data$class_b_probability <- current_evaluation_probabilities

    train_evaluation_data <- iteration_evaluation_data[iteration_evaluation_data$split == "train", , drop = FALSE]
    test_evaluation_data <- iteration_evaluation_data[iteration_evaluation_data$split == "test", , drop = FALSE]
    train_metrics <- calculate_classification_metrics(
      actual_labels = train_evaluation_data$class,
      predicted_labels = train_evaluation_data$predicted_class
    )
    test_metrics <- calculate_classification_metrics(
      actual_labels = test_evaluation_data$class,
      predicted_labels = test_evaluation_data$predicted_class
    )

    iteration_prediction_grid <- prediction_grid
    # The grid stores one predicted class and one probability per location.
    # build_classification_plot() later maps this grid to the heatmap.
    iteration_prediction_grid$predicted_class <- factor(
      grid_predictions,
      levels = c("Class A", "Class B")
    )
    iteration_prediction_grid$class_b_probability <- current_grid_probabilities

    list(
      loss = current_loss,
      accuracy = current_metrics$accuracy,
      iteration = list(
        iteration_index = iteration_index,
        weight_x = current_weight_x,
        weight_y = current_weight_y,
        bias = current_bias,
        training_probabilities = current_training_probabilities,
        training_predictions = training_predictions,
        classification_data = iteration_evaluation_data,
        prediction_grid = iteration_prediction_grid,
        loss_value = round(current_loss, 4),
        metrics = current_metrics,
        train_metrics = train_metrics,
        test_metrics = test_metrics
      )
    )
  }

  # Save iteration 0 before any update so playback can show the model starting
  # from zero weights and zero bias.
  initial_iteration <- build_saved_iteration(
    iteration_index = 0,
    current_weight_x = weight_x,
    current_weight_y = weight_y,
    current_bias = bias
  )
  saved_iteration_count <- 1
  iteration_history[[saved_iteration_count]] <- initial_iteration$iteration
  loss_history[saved_iteration_count] <- initial_iteration$loss
  accuracy_history[saved_iteration_count] <- initial_iteration$accuracy

  for (iteration_index in seq_len(total_iterations)) {
    # Step 1: use the current parameters to estimate Class B probabilities.
    linear_scores <- bias + weight_x * classification_data$x + weight_y * classification_data$y
    predicted_probabilities <- sigmoid_probability(linear_scores)

    # Step 2: measure how far the predictions are from the real labels.
    probability_error <- predicted_probabilities - binary_target

    # Step 3: compute one gradient descent update for plain binary
    # cross-entropy loss.
    gradient_weight_x <- mean(probability_error * classification_data$x)
    gradient_weight_y <- mean(probability_error * classification_data$y)
    gradient_bias <- mean(probability_error)

    # Gradient descent moves each parameter opposite the direction of error.
    weight_x <- weight_x - learning_rate * gradient_weight_x
    weight_y <- weight_y - learning_rate * gradient_weight_y
    if (fit_intercept) {
      bias <- bias - learning_rate * gradient_bias
    } else {
      # With fit_intercept off, the boundary is forced through the origin.
      bias <- 0
    }

    # Step 4: save the updated state so the UI can replay it later.
    saved_iteration <- build_saved_iteration(
      iteration_index = iteration_index,
      current_weight_x = weight_x,
      current_weight_y = weight_y,
      current_bias = bias
    )
    saved_iteration_count <- iteration_index + 1
    loss_history[saved_iteration_count] <- saved_iteration$loss
    accuracy_history[saved_iteration_count] <- saved_iteration$accuracy
    iteration_history[[saved_iteration_count]] <- saved_iteration$iteration

    actual_iteration_count <- iteration_index
  }

  iteration_history <- iteration_history[seq_len(saved_iteration_count)]
  loss_history <- loss_history[seq_len(saved_iteration_count)]
  accuracy_history <- accuracy_history[seq_len(saved_iteration_count)]

  final_iteration <- iteration_history[[saved_iteration_count]]

  # The returned structure is shared by plotting, metric cards, and playback.
  list(
    model_object = list(
      weight_x = final_iteration$weight_x,
      weight_y = final_iteration$weight_y,
      bias = final_iteration$bias,
      learning_rate = learning_rate,
      total_iterations = actual_iteration_count,
      stored_iteration_count = saved_iteration_count,
      requested_max_iter = total_iterations,
      fit_intercept = fit_intercept
    ),
    prediction_grid = final_iteration$prediction_grid,
    training_predictions = final_iteration$training_predictions,
    classification_data = final_iteration$classification_data,
    train_data = classification_data,
    test_data = evaluation_data[evaluation_data$split == "test", , drop = FALSE],
    metrics = final_iteration$train_metrics,
    train_metrics = final_iteration$train_metrics,
    test_metrics = final_iteration$test_metrics,
    split_counts = list(
      train = sum(evaluation_data$split == "train"),
      test = sum(evaluation_data$split == "test")
    ),
    iterations = iteration_history,
    iteration_metrics = data.frame(
      iteration = vapply(iteration_history, function(iteration_step) iteration_step$iteration_index, numeric(1)),
      loss = loss_history,
      accuracy = accuracy_history
    )
  )
}


