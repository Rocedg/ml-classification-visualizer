# k-NN backend helpers for the ML Visualizer app.
# cap_knn_k()
# Purpose:
#   Convert the requested k value into a safe neighbor count for the available
#   training rows.
cap_knn_k <- function(k_value, training_point_count) {
  if (is.null(k_value) || length(k_value) != 1 || is.na(k_value)) {
    k_value <- 5
  }

  numeric_k <- suppressWarnings(as.numeric(k_value))

  if (is.na(numeric_k) || !is.finite(numeric_k)) {
    numeric_k <- 5
  }

  if (is.null(training_point_count) || length(training_point_count) != 1 || is.na(training_point_count)) {
    training_point_count <- 0
  }

  training_point_count <- max(0, as.integer(training_point_count))

  if (training_point_count == 0) {
    return(0)
  }

  min(max(1, as.integer(round(numeric_k))), training_point_count)
}


# normalize_knn_distance_metric()
# Purpose:
#   Keep k-NN distance choices small, explicit, and safe.
normalize_knn_distance_metric <- function(distance_metric) {
  if (is.null(distance_metric) || length(distance_metric) != 1 || is.na(distance_metric)) {
    return("euclidean")
  }

  normalized_metric <- tolower(gsub("[ _-]+", "_", as.character(distance_metric)))

  switch(
    normalized_metric,
    euclidean = "euclidean",
    l2 = "euclidean",
    manhattan = "manhattan",
    l1 = "manhattan",
    "euclidean"
  )
}


# normalize_knn_voting_method()
# Purpose:
#   Keep k-NN voting choices small, explicit, and safe.
normalize_knn_voting_method <- function(voting_method) {
  if (is.null(voting_method) || length(voting_method) != 1 || is.na(voting_method)) {
    return("uniform")
  }

  normalized_method <- tolower(gsub("[ _-]+", "_", as.character(voting_method)))

  switch(
    normalized_method,
    uniform = "uniform",
    distance_weighted = "distance_weighted",
    weighted = "distance_weighted",
    "uniform"
  )
}


# calculate_knn_distances()
# Purpose:
#   Calculate the selected k-NN distance from one query point to all training
#   neighbors.
calculate_knn_distances <- function(query_x, query_y, neighbor_data, distance_metric = "euclidean") {
  distance_metric <- normalize_knn_distance_metric(distance_metric)

  if (distance_metric == "manhattan") {
    return(abs(query_x - neighbor_data$x) + abs(query_y - neighbor_data$y))
  }

  sqrt((query_x - neighbor_data$x)^2 + (query_y - neighbor_data$y)^2)
}


# summarize_knn_votes()
# Purpose:
#   Convert the nearest neighbor classes and distances into a predicted class
#   and binary probabilities using either uniform or inverse-distance voting.
summarize_knn_votes <- function(nearest_classes,
                                nearest_distances,
                                voting_method = "uniform",
                                class_levels = c("Class A", "Class B"),
                                epsilon = 1e-9) {
  voting_method <- normalize_knn_voting_method(voting_method)
  nearest_classes <- as.character(nearest_classes)
  nearest_distances <- suppressWarnings(as.numeric(nearest_distances))

  if (length(nearest_distances) != length(nearest_classes)) {
    nearest_distances <- rep(NA_real_, length(nearest_classes))
  }

  neighbor_weights <- rep(0, length(nearest_classes))
  valid_neighbors <- !is.na(nearest_classes) &
    nearest_classes %in% class_levels &
    !is.na(nearest_distances) &
    is.finite(nearest_distances)

  if (any(valid_neighbors)) {
    if (voting_method == "distance_weighted") {
      neighbor_weights[valid_neighbors] <- 1 / (pmax(nearest_distances[valid_neighbors], 0) + epsilon)
    } else {
      neighbor_weights[valid_neighbors] <- 1
    }
  }

  vote_totals <- stats::setNames(rep(0, length(class_levels)), class_levels)

  for (class_label in class_levels) {
    vote_totals[[class_label]] <- sum(neighbor_weights[nearest_classes == class_label], na.rm = TRUE)
  }

  total_vote_weight <- sum(vote_totals)

  if (total_vote_weight <= 0) {
    return(list(
      predicted_class = NA_character_,
      class_a_probability = NA_real_,
      class_b_probability = NA_real_,
      vote_totals = vote_totals,
      neighbor_weights = neighbor_weights,
      voting_method = voting_method
    ))
  }

  class_a_probability <- as.numeric(vote_totals[["Class A"]]) / total_vote_weight
  class_b_probability <- as.numeric(vote_totals[["Class B"]]) / total_vote_weight

  highest_vote_total <- max(vote_totals)
  tied_classes <- names(vote_totals[vote_totals == highest_vote_total])
  tied_classes <- tied_classes[tied_classes %in% nearest_classes]

  if (length(tied_classes) == 1) {
    predicted_class <- tied_classes
  } else if (length(tied_classes) > 1) {
    predicted_class <- nearest_classes[nearest_classes %in% tied_classes][1]
  } else {
    predicted_class <- names(vote_totals)[which.max(vote_totals)]
  }

  list(
    predicted_class = predicted_class,
    class_a_probability = class_a_probability,
    class_b_probability = class_b_probability,
    vote_totals = vote_totals,
    neighbor_weights = neighbor_weights,
    voting_method = voting_method
  )
}


# predict_knn_points()
# Purpose:
#   Predict labels and class probabilities for arbitrary x/y points using
#   nearest-neighbor voting from the training subset only.
# Inputs:
#   - training_data: rows allowed to vote as neighbors
#   - prediction_points: x/y rows to classify
#   - k: requested number of neighbors
#   - distance_metric: "euclidean" or "manhattan"
#   - voting_method: "uniform" or "distance_weighted"
# Output:
#   A list with predicted classes, Class A/Class B probabilities, and effective k.
predict_knn_points <- function(training_data,
                               prediction_points,
                               k = 5,
                               distance_metric = "euclidean",
                               voting_method = "uniform") {
  class_levels <- c("Class A", "Class B")
  distance_metric <- normalize_knn_distance_metric(distance_metric)
  voting_method <- normalize_knn_voting_method(voting_method)
  prediction_count <- nrow(prediction_points)

  predicted_classes <- rep(NA_character_, prediction_count)
  class_a_probabilities <- rep(NA_real_, prediction_count)

  if (prediction_count == 0) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      class_a_probability = class_a_probabilities,
      class_b_probability = class_a_probabilities,
      effective_k = 0,
      distance_metric = distance_metric,
      voting_method = voting_method
    ))
  }

  required_training_columns <- c("x", "y", "class")
  required_prediction_columns <- c("x", "y")

  if (!all(required_training_columns %in% names(training_data)) ||
      !all(required_prediction_columns %in% names(prediction_points))) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      class_a_probability = class_a_probabilities,
      class_b_probability = class_a_probabilities,
      effective_k = 0,
      distance_metric = distance_metric,
      voting_method = voting_method
    ))
  }

  valid_training_rows <- stats::complete.cases(training_data[, required_training_columns, drop = FALSE])
  neighbor_data <- training_data[valid_training_rows, required_training_columns, drop = FALSE]
  neighbor_data$class <- factor(as.character(neighbor_data$class), levels = class_levels)
  neighbor_data <- neighbor_data[!is.na(neighbor_data$class), , drop = FALSE]

  effective_k <- cap_knn_k(k, nrow(neighbor_data))

  if (effective_k == 0) {
    return(list(
      predicted_class = factor(predicted_classes, levels = class_levels),
      class_a_probability = class_a_probabilities,
      class_b_probability = class_a_probabilities,
      effective_k = effective_k,
      distance_metric = distance_metric,
      voting_method = voting_method
    ))
  }

  for (point_index in seq_len(prediction_count)) {
    point_x <- prediction_points$x[point_index]
    point_y <- prediction_points$y[point_index]

    if (is.na(point_x) || is.na(point_y)) {
      next
    }

    distances <- calculate_knn_distances(
      query_x = point_x,
      query_y = point_y,
      neighbor_data = neighbor_data,
      distance_metric = distance_metric
    )
    ordered_neighbor_indices <- order(distances, seq_along(distances), na.last = NA)

    if (length(ordered_neighbor_indices) == 0) {
      next
    }

    nearest_indices <- head(ordered_neighbor_indices, effective_k)
    nearest_classes <- as.character(neighbor_data$class[nearest_indices])
    vote_summary <- summarize_knn_votes(
      nearest_classes = nearest_classes,
      nearest_distances = distances[nearest_indices],
      voting_method = voting_method,
      class_levels = class_levels
    )

    predicted_classes[point_index] <- vote_summary$predicted_class
    class_a_probabilities[point_index] <- vote_summary$class_a_probability
  }

  list(
    predicted_class = factor(predicted_classes, levels = class_levels),
    class_a_probability = class_a_probabilities,
    class_b_probability = 1 - class_a_probabilities,
    effective_k = effective_k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )
}


# find_knn_neighbors()
# Purpose:
#   Return the nearest training rows for one query point, including rank and
#   distance values used by the inspection panel.
find_knn_neighbors <- function(training_data, query_point, k = 5, distance_metric = "euclidean") {
  class_levels <- c("Class A", "Class B")
  distance_metric <- normalize_knn_distance_metric(distance_metric)
  empty_neighbors <- data.frame(
    rank = integer(0),
    x = numeric(0),
    y = numeric(0),
    class = factor(character(0), levels = class_levels),
    distance = numeric(0)
  )

  if (is.null(query_point) || !all(c("x", "y") %in% names(query_point))) {
    return(empty_neighbors)
  }

  query_x <- suppressWarnings(as.numeric(query_point$x[1]))
  query_y <- suppressWarnings(as.numeric(query_point$y[1]))

  if (is.na(query_x) || is.na(query_y)) {
    return(empty_neighbors)
  }

  required_training_columns <- c("x", "y", "class")

  if (is.null(training_data) || !all(required_training_columns %in% names(training_data))) {
    return(empty_neighbors)
  }

  valid_training_rows <- stats::complete.cases(training_data[, required_training_columns, drop = FALSE])
  neighbor_data <- training_data[valid_training_rows, required_training_columns, drop = FALSE]
  neighbor_data$class <- factor(as.character(neighbor_data$class), levels = class_levels)
  neighbor_data <- neighbor_data[!is.na(neighbor_data$class), , drop = FALSE]

  effective_k <- cap_knn_k(k, nrow(neighbor_data))

  if (effective_k == 0) {
    return(empty_neighbors)
  }

  distances <- calculate_knn_distances(
    query_x = query_x,
    query_y = query_y,
    neighbor_data = neighbor_data,
    distance_metric = distance_metric
  )
  ordered_neighbor_indices <- order(distances, seq_along(distances), na.last = NA)

  if (length(ordered_neighbor_indices) == 0) {
    return(empty_neighbors)
  }

  nearest_indices <- head(ordered_neighbor_indices, effective_k)
  nearest_neighbors <- neighbor_data[nearest_indices, , drop = FALSE]

  data.frame(
    rank = seq_along(nearest_indices),
    x = nearest_neighbors$x,
    y = nearest_neighbors$y,
    class = factor(as.character(nearest_neighbors$class), levels = class_levels),
    distance = distances[nearest_indices],
    stringsAsFactors = FALSE
  )
}


# inspect_knn_point()
# Purpose:
#   Build the compact details needed to explain a k-NN prediction at one
#   selected plot location.
inspect_knn_point <- function(training_data,
                              query_point,
                              k = 5,
                              distance_metric = "euclidean",
                              voting_method = "uniform") {
  class_levels <- c("Class A", "Class B")
  distance_metric <- normalize_knn_distance_metric(distance_metric)
  voting_method <- normalize_knn_voting_method(voting_method)

  if (is.null(query_point) || !all(c("x", "y") %in% names(query_point))) {
    return(NULL)
  }

  query_data <- data.frame(
    x = suppressWarnings(as.numeric(query_point$x[1])),
    y = suppressWarnings(as.numeric(query_point$y[1]))
  )

  if (is.na(query_data$x) || is.na(query_data$y)) {
    return(NULL)
  }

  nearest_neighbors <- find_knn_neighbors(
    training_data = training_data,
    query_point = query_data,
    k = k,
    distance_metric = distance_metric
  )
  prediction_result <- predict_knn_points(
    training_data = training_data,
    prediction_points = query_data,
    k = k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )

  vote_counts <- table(factor(as.character(nearest_neighbors$class), levels = class_levels))
  vote_summary <- summarize_knn_votes(
    nearest_classes = as.character(nearest_neighbors$class),
    nearest_distances = nearest_neighbors$distance,
    voting_method = voting_method,
    class_levels = class_levels
  )
  nearest_neighbors$weight <- vote_summary$neighbor_weights
  predicted_class <- as.character(prediction_result$predicted_class[1])

  if (is.na(predicted_class)) {
    predicted_class <- NA_character_
  }

  list(
    query_point = query_data,
    predicted_class = predicted_class,
    vote_counts = vote_counts,
    weighted_votes = vote_summary$vote_totals,
    neighbors = nearest_neighbors,
    effective_k = prediction_result$effective_k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )
}


# train_knn_classifier()
# Purpose:
#   Store the training points and evaluate k-NN predictions for train/test rows
#   and the main prediction grid.
train_knn_classifier <- function(classification_data,
                                 prediction_grid,
                                 k = 5,
                                 distance_metric = "euclidean",
                                 voting_method = "uniform",
                                 evaluation_data = classification_data) {
  distance_metric <- normalize_knn_distance_metric(distance_metric)
  voting_method <- normalize_knn_voting_method(voting_method)

  if (!"split" %in% names(evaluation_data)) {
    evaluation_data$split <- factor(rep("train", nrow(evaluation_data)), levels = c("train", "test"))
  } else {
    evaluation_data$split <- factor(as.character(evaluation_data$split), levels = c("train", "test"))
  }

  valid_training_rows <- stats::complete.cases(classification_data[, c("x", "y", "class"), drop = FALSE])

  if (sum(valid_training_rows) == 0) {
    stop("k-NN needs at least one complete training point.")
  }

  knn_grid_predictions <- predict_knn_points(
    training_data = classification_data,
    prediction_points = prediction_grid,
    k = k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )
  knn_evaluation_predictions <- predict_knn_points(
    training_data = classification_data,
    prediction_points = evaluation_data,
    k = k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )
  knn_training_predictions <- predict_knn_points(
    training_data = classification_data,
    prediction_points = classification_data,
    k = k,
    distance_metric = distance_metric,
    voting_method = voting_method
  )

  evaluation_results <- evaluation_data
  evaluation_results$predicted_class <- knn_evaluation_predictions$predicted_class
  evaluation_results$class_a_probability <- knn_evaluation_predictions$class_a_probability
  evaluation_results$class_b_probability <- knn_evaluation_predictions$class_b_probability

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

  grid_results <- prediction_grid
  grid_results$predicted_class <- knn_grid_predictions$predicted_class
  grid_results$class_a_probability <- knn_grid_predictions$class_a_probability
  grid_results$class_b_probability <- knn_grid_predictions$class_b_probability

  list(
    model_object = list(
      requested_k = k,
      effective_k = knn_grid_predictions$effective_k,
      distance_metric = distance_metric,
      voting_method = voting_method,
      training_point_count = sum(valid_training_rows),
      class_counts = table(factor(classification_data$class, levels = c("Class A", "Class B")))
    ),
    prediction_grid = grid_results,
    training_predictions = knn_training_predictions$predicted_class,
    classification_data = evaluation_results,
    train_data = classification_data,
    test_data = evaluation_data[evaluation_data$split == "test", , drop = FALSE],
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


