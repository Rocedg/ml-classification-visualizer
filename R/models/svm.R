# SVM backend helpers for the ML Visualizer app.

# normalize_svm_kernel()
# Purpose:
#   Map UI labels to the e1071 kernel names supported in this app.
normalize_svm_kernel <- function(kernel_value) {
  if (is.null(kernel_value) || length(kernel_value) != 1 || is.na(kernel_value)) {
    return("linear")
  }

  normalized_kernel <- tolower(gsub("[ _-]+", "_", as.character(kernel_value)))

  switch(
    normalized_kernel,
    linear = "linear",
    radial = "radial",
    rbf = "radial",
    polynomial = "polynomial",
    poly = "polynomial",
    "linear"
  )
}


svm_kernel_display_label <- function(kernel_value) {
  switch(
    normalize_svm_kernel(kernel_value),
    linear = "Linear",
    radial = "RBF",
    polynomial = "Polynomial",
    "Linear"
  )
}

# normalize_svm_cost()
# Purpose:
#   Keep the SVM C / cost value numeric, positive, and beginner-friendly.
normalize_svm_cost <- function(cost_value) {
  if (is.null(cost_value) || length(cost_value) != 1 || is.na(cost_value)) {
    return(1)
  }

  numeric_cost <- suppressWarnings(as.numeric(cost_value))

  if (is.na(numeric_cost) || !is.finite(numeric_cost) || numeric_cost <= 0) {
    stop("C / Cost must be a positive numeric value.")
  }

  numeric_cost
}


# normalize_svm_gamma()
# Purpose:
#   Validate gamma for kernels that use local similarity or polynomial terms.
normalize_svm_gamma <- function(gamma_value) {
  if (is.null(gamma_value) || length(gamma_value) != 1 || is.na(gamma_value)) {
    return(0.5)
  }

  numeric_gamma <- suppressWarnings(as.numeric(gamma_value))

  if (is.na(numeric_gamma) || !is.finite(numeric_gamma) || numeric_gamma <= 0) {
    stop("Gamma must be a positive numeric value.")
  }

  numeric_gamma
}


# normalize_svm_degree()
# Purpose:
#   Keep polynomial degree as a small positive integer for beginner-friendly UI.
normalize_svm_degree <- function(degree_value) {
  if (is.null(degree_value) || length(degree_value) != 1 || is.na(degree_value)) {
    return(3)
  }

  numeric_degree <- suppressWarnings(as.numeric(degree_value))

  if (is.na(numeric_degree) || !is.finite(numeric_degree) || numeric_degree < 1) {
    stop("Degree must be a positive integer.")
  }

  as.integer(round(numeric_degree))
}


# svm_fit_model()
# Purpose:
#   Fit an e1071 SVM on the training split only.
svm_fit_model <- function(training_data,
                          cost = 1,
                          kernel = "linear",
                          gamma = 0.5,
                          degree = 3) {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("SVM requires the e1071 package. Install it with install.packages('e1071').")
  }

  cost <- normalize_svm_cost(cost)
  kernel <- normalize_svm_kernel(kernel)
  gamma <- if (kernel %in% c("radial", "polynomial")) normalize_svm_gamma(gamma) else 0.5
  degree <- if (identical(kernel, "polynomial")) normalize_svm_degree(degree) else 3
  class_levels <- c("Class A", "Class B")
  required_columns <- c("x", "y", "class")

  if (is.null(training_data) || !all(required_columns %in% names(training_data))) {
    stop("SVM needs training data with x, y, and class columns.")
  }

  valid_training_rows <- stats::complete.cases(training_data[, required_columns, drop = FALSE])
  svm_training_data <- training_data[valid_training_rows, required_columns, drop = FALSE]
  svm_training_data$class <- factor(as.character(svm_training_data$class), levels = class_levels)
  svm_training_data <- svm_training_data[!is.na(svm_training_data$class), , drop = FALSE]

  if (nrow(svm_training_data) < 2) {
    stop("SVM needs at least 2 complete training points.")
  }

  if (length(unique(svm_training_data$class)) < 2) {
    stop("The training split must contain both Class A and Class B before fitting SVM.")
  }

  train_x <- svm_training_data[, c("x", "y"), drop = FALSE]
  train_y <- svm_training_data$class

  svm_args <- list(
    x = train_x,
    y = train_y,
    type = "C-classification",
    kernel = kernel,
    cost = cost,
    scale = FALSE
  )

  if (kernel %in% c("radial", "polynomial")) {
    svm_args$gamma <- gamma
  }

  if (identical(kernel, "polynomial")) {
    svm_args$degree <- degree
  }

  fitted_svm <- tryCatch(
    do.call(e1071::svm, svm_args),
    error = function(error_object) {
      stop(paste(svm_kernel_display_label(kernel), "SVM could not be fitted:", error_object$message), call. = FALSE)
    }
  )

  support_vectors <- svm_get_support_vectors(
    fitted_svm = fitted_svm,
    training_data = svm_training_data
  )

  list(
    fit = fitted_svm,
    kernel = kernel,
    cost = cost,
    gamma = if (kernel %in% c("radial", "polynomial")) gamma else NULL,
    degree = if (identical(kernel, "polynomial")) degree else NULL,
    class_levels = class_levels,
    training_data = svm_training_data,
    support_vectors = support_vectors
  )
}


# svm_fit_linear()
# Purpose:
#   Compatibility wrapper for the first SVM version's linear fit helper.
svm_fit_linear <- function(training_data, cost = 1) {
  svm_fit_model(
    training_data = training_data,
    cost = cost,
    kernel = "linear"
  )
}


# svm_get_decision_values()
# Purpose:
#   Extract the binary decision function scores returned by e1071::predict().
svm_get_decision_values <- function(predictions, prediction_count) {
  decision_values <- attr(predictions, "decision.values")

  if (is.null(decision_values)) {
    return(rep(NA_real_, prediction_count))
  }

  if (is.matrix(decision_values) || is.data.frame(decision_values)) {
    decision_values <- decision_values[, 1]
  }

  decision_values <- suppressWarnings(as.numeric(decision_values))

  if (length(decision_values) != prediction_count) {
    return(rep(NA_real_, prediction_count))
  }

  decision_values
}


# svm_predict()
# Purpose:
#   Predict class labels and decision scores for arbitrary x/y points.
svm_predict <- function(model_object, prediction_points) {
  class_levels <- c("Class A", "Class B")
  prediction_count <- if (is.null(prediction_points)) 0 else nrow(prediction_points)
  predicted_classes <- rep(NA_character_, prediction_count)
  decision_values <- rep(NA_real_, prediction_count)

  if (prediction_count == 0) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      decision_value = decision_values
    ))
  }

  if (is.null(model_object) || is.null(model_object$fit)) {
    stop("SVM predictions need a fitted model.")
  }

  required_prediction_columns <- c("x", "y")

  if (!all(required_prediction_columns %in% names(prediction_points))) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      decision_value = decision_values
    ))
  }

  valid_prediction_rows <- stats::complete.cases(prediction_points[, required_prediction_columns, drop = FALSE])

  if (!any(valid_prediction_rows)) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      decision_value = decision_values
    ))
  }

  prediction_data <- data.frame(
    x = as.numeric(prediction_points$x[valid_prediction_rows]),
    y = as.numeric(prediction_points$y[valid_prediction_rows])
  )

  predictions <- tryCatch(
    stats::predict(model_object$fit, prediction_data, decision.values = TRUE),
    error = function(error_object) {
      stop(paste(svm_kernel_display_label(model_object$kernel), "SVM prediction failed:", error_object$message), call. = FALSE)
    }
  )

  predicted_classes[valid_prediction_rows] <- as.character(predictions)
  decision_values[valid_prediction_rows] <- svm_get_decision_values(
    predictions = predictions,
    prediction_count = nrow(prediction_data)
  )

  list(
    predicted_class = factor(predicted_classes, levels = class_levels),
    decision_value = decision_values
  )
}


# svm_predict_grid()
# Purpose:
#   Add SVM predicted classes and decision scores to the plot grid.
svm_predict_grid <- function(model_object, prediction_grid) {
  grid_predictions <- svm_predict(
    model_object = model_object,
    prediction_points = prediction_grid
  )

  grid_results <- prediction_grid
  grid_results$predicted_class <- grid_predictions$predicted_class
  grid_results$decision_value <- grid_predictions$decision_value
  grid_results$class_a_probability <- NA_real_
  grid_results$class_b_probability <- NA_real_
  grid_results
}


# svm_get_support_vectors()
# Purpose:
#   Map e1071 support vector row indexes back to the training data.
svm_get_support_vectors <- function(fitted_svm, training_data) {
  class_levels <- c("Class A", "Class B")
  empty_support_vectors <- data.frame(
    x = numeric(0),
    y = numeric(0),
    class = factor(character(0), levels = class_levels),
    stringsAsFactors = FALSE
  )

  if (is.null(fitted_svm) || is.null(training_data) || nrow(training_data) == 0) {
    return(empty_support_vectors)
  }

  support_indexes <- fitted_svm$index

  if (!is.null(support_indexes) && length(support_indexes) > 0) {
    support_indexes <- support_indexes[support_indexes >= 1 & support_indexes <= nrow(training_data)]

    if (length(support_indexes) > 0) {
      support_vectors <- training_data[support_indexes, , drop = FALSE]
      support_vectors$class <- factor(as.character(support_vectors$class), levels = class_levels)
      return(support_vectors[, c("x", "y", "class"), drop = FALSE])
    }
  }

  if (!is.null(fitted_svm$SV) && all(c("x", "y") %in% colnames(fitted_svm$SV))) {
    support_vectors <- data.frame(
      x = as.numeric(fitted_svm$SV[, "x"]),
      y = as.numeric(fitted_svm$SV[, "y"]),
      class = factor(rep(NA_character_, nrow(fitted_svm$SV)), levels = class_levels),
      stringsAsFactors = FALSE
    )
    return(support_vectors)
  }

  empty_support_vectors
}


# svm_margin_summary()
# Purpose:
#   Summarize the key educational SVM quantities for UI panels.
svm_margin_summary <- function(model_object, training_results = NULL) {
  support_vector_count <- 0

  if (!is.null(model_object$support_vectors)) {
    support_vector_count <- nrow(model_object$support_vectors)
  }

  misclassified_train_points <- NA_integer_
  margin_violation_count <- NA_integer_

  if (!is.null(training_results)) {
    if (all(c("class", "predicted_class") %in% names(training_results))) {
      valid_prediction_rows <- !is.na(training_results$class) & !is.na(training_results$predicted_class)
      misclassified_train_points <- sum(
        as.character(training_results$class[valid_prediction_rows]) !=
          as.character(training_results$predicted_class[valid_prediction_rows])
      )
    }

    if ("decision_value" %in% names(training_results)) {
      valid_decision_values <- !is.na(training_results$decision_value) & is.finite(training_results$decision_value)
      margin_violation_count <- sum(abs(training_results$decision_value[valid_decision_values]) < 1)
    }
  }

  list(
    kernel = model_object$kernel,
    kernel_label = svm_kernel_display_label(model_object$kernel),
    cost = model_object$cost,
    gamma = model_object$gamma,
    degree = model_object$degree,
    support_vector_count = support_vector_count,
    decision_boundary = "score = 0",
    margins = "score = -1 and +1",
    margin_violation_count = margin_violation_count,
    misclassified_train_points = misclassified_train_points
  )
}


# train_svm_classifier()
# Purpose:
#   Fit an SVM and evaluate it for train/test rows and the plot grid.
train_svm_classifier <- function(classification_data,
                                 prediction_grid,
                                 cost = 1,
                                 kernel = "linear",
                                 gamma = 0.5,
                                 degree = 3,
                                 evaluation_data = classification_data) {
  if (!"split" %in% names(evaluation_data)) {
    evaluation_data$split <- factor(rep("train", nrow(evaluation_data)), levels = c("train", "test"))
  } else {
    evaluation_data$split <- factor(as.character(evaluation_data$split), levels = c("train", "test"))
  }

  model_object <- svm_fit_model(
    training_data = classification_data,
    cost = cost,
    kernel = kernel,
    gamma = gamma,
    degree = degree
  )

  grid_results <- svm_predict_grid(
    model_object = model_object,
    prediction_grid = prediction_grid
  )

  evaluation_predictions <- svm_predict(
    model_object = model_object,
    prediction_points = evaluation_data
  )
  training_predictions <- svm_predict(
    model_object = model_object,
    prediction_points = classification_data
  )

  evaluation_results <- evaluation_data
  evaluation_results$predicted_class <- evaluation_predictions$predicted_class
  evaluation_results$decision_value <- evaluation_predictions$decision_value
  evaluation_results$class_a_probability <- NA_real_
  evaluation_results$class_b_probability <- NA_real_

  train_evaluation_data <- evaluation_results[evaluation_results$split == "train", , drop = FALSE]
  test_evaluation_data <- evaluation_results[evaluation_results$split == "test", , drop = FALSE]

  train_metrics <- calculate_classification_metrics(
    actual_labels = train_evaluation_data$class,
    predicted_labels = train_evaluation_data$predicted_class
  )
  test_metrics <- calculate_classification_metrics(
    actual_labels = test_evaluation_data$class,
    predicted_labels = test_evaluation_data$predicted_class
  )

  training_results <- classification_data
  training_results$predicted_class <- training_predictions$predicted_class
  training_results$decision_value <- training_predictions$decision_value
  model_object$margin_summary <- svm_margin_summary(
    model_object = model_object,
    training_results = training_results
  )

  list(
    model_object = model_object,
    prediction_grid = grid_results,
    training_predictions = training_predictions$predicted_class,
    classification_data = evaluation_results,
    train_data = classification_data,
    test_data = evaluation_data[evaluation_data$split == "test", , drop = FALSE],
    support_vectors = model_object$support_vectors,
    margin_summary = model_object$margin_summary,
    metrics = train_metrics,
    train_metrics = train_metrics,
    test_metrics = test_metrics,
    split_counts = list(
      train = sum(evaluation_data$split == "train"),
      test = sum(evaluation_data$split == "test")
    ),
    iterations = NULL,
    iteration_metrics = NULL
  )
}
