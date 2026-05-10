# Model training helper functions for the ML Visualizer app.

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


# create_train_test_split()
# Purpose:
#   Mark the current run data with a reproducible 70/30 train/test split.
# Inputs:
#   - classification_data: current Class A / Class B points
#   - train_fraction: share of points used to fit the model
#   - seed: fixed seed so the same run data gets the same split
# Output:
#   The same data with a split column containing "train" or "test".
create_train_test_split <- function(classification_data, train_fraction = 0.70, seed = 42) {
  split_data <- classification_data
  split_data$split <- factor(rep("train", nrow(split_data)), levels = c("train", "test"))

  if (nrow(split_data) < 2) {
    return(split_data)
  }

  train_fraction <- min(max(train_fraction, 0.01), 0.99)

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)

  train_indices <- integer(0)
  test_indices <- integer(0)
  observed_classes <- unique(as.character(split_data$class))

  for (class_label in observed_classes) {
    class_indices <- which(as.character(split_data$class) == class_label)
    class_count <- length(class_indices)

    if (class_count == 0) {
      next
    }

    if (class_count == 1) {
      train_indices <- c(train_indices, class_indices)
      next
    }

    class_train_count <- floor(class_count * train_fraction)
    class_train_count <- min(max(class_train_count, 1), class_count - 1)
    class_train_indices <- sample(class_indices, size = class_train_count)

    train_indices <- c(train_indices, class_train_indices)
    test_indices <- c(test_indices, setdiff(class_indices, class_train_indices))
  }

  missing_train_classes <- setdiff(
    observed_classes,
    unique(as.character(split_data$class[train_indices]))
  )

  for (class_label in missing_train_classes) {
    candidate_index <- test_indices[as.character(split_data$class[test_indices]) == class_label][1]

    if (!is.na(candidate_index)) {
      train_indices <- c(train_indices, candidate_index)
      test_indices <- setdiff(test_indices, candidate_index)
    }
  }

  if (length(test_indices) == 0 && length(train_indices) > 1) {
    train_class_counts <- table(as.character(split_data$class[train_indices]))
    movable_classes <- names(train_class_counts[train_class_counts > 1])

    if (length(movable_classes) > 0) {
      movable_index <- train_indices[as.character(split_data$class[train_indices]) == movable_classes[1]][1]
      train_indices <- setdiff(train_indices, movable_index)
      test_indices <- c(test_indices, movable_index)
    }
  }

  split_data$split <- factor(rep("test", nrow(split_data)), levels = c("train", "test"))
  split_data$split[train_indices] <- "train"
  split_data$split <- factor(as.character(split_data$split), levels = c("train", "test"))
  split_data
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
  if (nrow(classification_data) < 6) {
    stop("Please provide at least 6 data points before training a classifier.")
  }

  if (length(unique(classification_data$class)) < 2) {
    stop("The dataset must contain both Class A and Class B.")
  }

  split_classification_data <- create_train_test_split(classification_data)
  train_classification_data <- split_classification_data[split_classification_data$split == "train", , drop = FALSE]

  if (nrow(train_classification_data) < 2) {
    stop("The training split needs at least 2 data points. Add more data before running the classifier.")
  }

  if (length(unique(train_classification_data$class)) < 2) {
    stop("The training split must contain both Class A and Class B. Add more balanced data before running the classifier.")
  }

  # The grid is created once for this run from all points. Every saved
  # iteration writes its probabilities into the same coordinate layout.
  prediction_grid <- build_prediction_grid(split_classification_data)

  if (algorithm_name == "logistic_regression") {
    if (is.null(parameter_values)) {
      parameter_values <- list()
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
  } else {
    stop("Only Logistic Regression is currently available for training.")
  }

  # SVM and k-NN training branches can be restored here when those
  # algorithms are ready to be enabled again in the UI.
  trained_model <- logistic_training_results$model_object
  prediction_grid <- logistic_training_results$prediction_grid
  training_predictions <- logistic_training_results$training_predictions
  metrics <- logistic_training_results$metrics
  train_metrics <- logistic_training_results$train_metrics
  test_metrics <- logistic_training_results$test_metrics
  split_counts <- logistic_training_results$split_counts
  iteration_history <- logistic_training_results$iterations
  iteration_metrics <- logistic_training_results$iteration_metrics

  list(
    algorithm_key = algorithm_name,
    algorithm_label = algorithm_label,
    model_object = trained_model,
    classification_data = logistic_training_results$classification_data,
    train_data = logistic_training_results$train_data,
    test_data = logistic_training_results$test_data,
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
