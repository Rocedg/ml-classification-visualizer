# Data helper functions for the ML Visualizer app.

create_indexed_dataset <- function(x_values, y_values, class_labels) {
  classification_data <- data.frame(
    index = seq_along(x_values),
    x = round(as.numeric(x_values), 3),
    y = round(as.numeric(y_values), 3),
    class = factor(as.character(class_labels), levels = c("Class A", "Class B")),
    stringsAsFactors = FALSE
  )

  classification_data
}


create_empty_classification_data <- function() {
  data.frame(
    index = integer(0),
    x = numeric(0),
    y = numeric(0),
    class = factor(character(0), levels = c("Class A", "Class B")),
    stringsAsFactors = FALSE
  )
}


generate_preset_dataset <- function(dataset_name, points_per_class = 40) {
  seed_lookup <- c(
    "Gaussian clusters" = 101,
    "Linearly separable" = 202,
    "Overlapping classes" = 303,
    "Moons" = 404,
    "Circles" = 505
  )

  set.seed(seed_lookup[[dataset_name]])

  if (dataset_name == "Gaussian clusters") {
    class_a_x <- rnorm(points_per_class, mean = -2.3, sd = 1.0)
    class_a_y <- rnorm(points_per_class, mean = 2.4, sd = 1.1)
    class_b_x <- rnorm(points_per_class, mean = 2.4, sd = 1.0)
    class_b_y <- rnorm(points_per_class, mean = -2.2, sd = 1.1)
  } else if (dataset_name == "Linearly separable") {
    class_a_x <- runif(points_per_class, min = -5, max = 1.2)
    class_a_y <- 0.7 * class_a_x + rnorm(points_per_class, mean = -1.5, sd = 0.8)

    class_b_x <- runif(points_per_class, min = -1.2, max = 5)
    class_b_y <- 0.7 * class_b_x + rnorm(points_per_class, mean = 1.7, sd = 0.8)
  } else if (dataset_name == "Overlapping classes") {
    class_a_x <- rnorm(points_per_class, mean = -1.2, sd = 1.6)
    class_a_y <- rnorm(points_per_class, mean = 0.9, sd = 1.5)
    class_b_x <- rnorm(points_per_class, mean = 1.1, sd = 1.6)
    class_b_y <- rnorm(points_per_class, mean = -0.8, sd = 1.5)
  } else if (dataset_name == "Moons") {
    moon_angle_a <- runif(points_per_class, min = 0, max = pi)
    moon_angle_b <- runif(points_per_class, min = 0, max = pi)

    class_a_x <- 3.2 * cos(moon_angle_a) + rnorm(points_per_class, sd = 0.18)
    class_a_y <- 3.2 * sin(moon_angle_a) + rnorm(points_per_class, sd = 0.18)

    class_b_x <- 3.2 * (1 - cos(moon_angle_b)) + rnorm(points_per_class, sd = 0.18)
    class_b_y <- -3.2 * sin(moon_angle_b) + 1.1 + rnorm(points_per_class, sd = 0.18)
  } else if (dataset_name == "Circles") {
    outer_angle <- runif(points_per_class, min = 0, max = 2 * pi)
    inner_angle <- runif(points_per_class, min = 0, max = 2 * pi)

    class_a_x <- 4.0 * cos(outer_angle) + rnorm(points_per_class, sd = 0.25)
    class_a_y <- 4.0 * sin(outer_angle) + rnorm(points_per_class, sd = 0.25)

    class_b_x <- 1.9 * cos(inner_angle) + rnorm(points_per_class, sd = 0.18)
    class_b_y <- 1.9 * sin(inner_angle) + rnorm(points_per_class, sd = 0.18)
  } else {
    stop("Unknown preset dataset selected.")
  }

  x_values <- c(class_a_x, class_b_x)
  y_values <- c(class_a_y, class_b_y)
  class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  create_indexed_dataset(x_values, y_values, class_labels)
}


convert_uploaded_csv_to_dataset <- function(uploaded_table) {
  cleaned_names <- tolower(names(uploaded_table))
  names(uploaded_table) <- cleaned_names

  required_columns <- c("x", "y", "class")
  missing_columns <- setdiff(required_columns, names(uploaded_table))

  if (length(missing_columns) > 0) {
    stop("Uploaded CSV must contain the columns: x, y, and class.")
  }

  cleaned_table <- uploaded_table[, required_columns]
  cleaned_table <- cleaned_table[stats::complete.cases(cleaned_table), , drop = FALSE]

  cleaned_table$x <- as.numeric(cleaned_table$x)
  cleaned_table$y <- as.numeric(cleaned_table$y)

  if (any(is.na(cleaned_table$x)) || any(is.na(cleaned_table$y))) {
    stop("Columns x and y must contain numeric values.")
  }

  unique_classes <- unique(as.character(cleaned_table$class))

  if (length(unique_classes) != 2) {
    stop("Uploaded CSV must contain exactly two distinct class labels.")
  }

  standardized_class_labels <- ifelse(
    as.character(cleaned_table$class) == unique_classes[1],
    "Class A",
    "Class B"
  )

  create_indexed_dataset(cleaned_table$x, cleaned_table$y, standardized_class_labels)
}


build_prediction_grid <- function(classification_data, grid_points = 140) {
  x_padding <- diff(range(classification_data$x)) * 0.18
  y_padding <- diff(range(classification_data$y)) * 0.18

  if (x_padding == 0) x_padding <- 1
  if (y_padding == 0) y_padding <- 1

  x_sequence <- seq(
    min(classification_data$x) - x_padding,
    max(classification_data$x) + x_padding,
    length.out = grid_points
  )

  y_sequence <- seq(
    min(classification_data$y) - y_padding,
    max(classification_data$y) + y_padding,
    length.out = grid_points
  )

  expand.grid(
    x = x_sequence,
    y = y_sequence
  )
}
