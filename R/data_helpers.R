# Data helper functions for the ML Visualizer app.

# create_indexed_dataset()
# Purpose:
#   Build the standard two-class data frame used throughout the app.
# Inputs:
#   - x_values, y_values: numeric coordinates for points on the plot
#   - class_labels: labels that identify each point as Class A or Class B
# Output:
#   A data frame with stable row indexes, rounded coordinates, and class factors.
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


# create_empty_classification_data()
# Purpose:
#   Return an empty data frame with the same columns as a real dataset.
# Used by:
#   Drawing controls, where custom points may start empty and grow over time.
create_empty_classification_data <- function() {
  data.frame(
    index = integer(0),
    x = numeric(0),
    y = numeric(0),
    class = factor(character(0), levels = c("Class A", "Class B")),
    stringsAsFactors = FALSE
  )
}


# generate_preset_dataset()
# Purpose:
#   Create reproducible example datasets for the visualizer sidebar.
# Inputs:
#   - dataset_name: selected preset name from the UI
#   - points_per_class: maximum number of examples to sample per class
# Output:
#   A standardized Class A / Class B dataset created by create_indexed_dataset().
generate_preset_dataset <- function(dataset_name, points_per_class = 40) {
  seed_lookup <- c(
    "Gaussian clusters" = 101,
    "Linearly separable" = 202,
    "Overlapping classes" = 303,
    "Moons" = 404,
    "Circles" = 505,
    "Titanic passengers" = 606,
    "Diabetes health data" = 707
  )

  if (!dataset_name %in% names(seed_lookup)) {
    stop("Unknown preset dataset selected.")
  }

  set.seed(seed_lookup[[dataset_name]])

  # Synthetic presets are generated directly from random distributions.
  # The fixed seed above keeps each preset stable between app sessions.
  if (dataset_name == "Gaussian clusters") {
    class_a_x <- rnorm(points_per_class, mean = -2.3, sd = 1.0)
    class_a_y <- rnorm(points_per_class, mean = 2.4, sd = 1.1)
    class_b_x <- rnorm(points_per_class, mean = 2.4, sd = 1.0)
    class_b_y <- rnorm(points_per_class, mean = -2.2, sd = 1.1)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Linearly separable") {
    class_a_x <- runif(points_per_class, min = -5, max = 1.2)
    class_a_y <- 0.7 * class_a_x + rnorm(points_per_class, mean = -1.5, sd = 0.8)

    class_b_x <- runif(points_per_class, min = -1.2, max = 5)
    class_b_y <- 0.7 * class_b_x + rnorm(points_per_class, mean = 1.7, sd = 0.8)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Overlapping classes") {
    class_a_x <- rnorm(points_per_class, mean = -1.2, sd = 1.6)
    class_a_y <- rnorm(points_per_class, mean = 0.9, sd = 1.5)
    class_b_x <- rnorm(points_per_class, mean = 1.1, sd = 1.6)
    class_b_y <- rnorm(points_per_class, mean = -0.8, sd = 1.5)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Moons") {
    moon_angle_a <- runif(points_per_class, min = 0, max = pi)
    moon_angle_b <- runif(points_per_class, min = 0, max = pi)

    class_a_x <- 3.2 * cos(moon_angle_a) + rnorm(points_per_class, sd = 0.18)
    class_a_y <- 3.2 * sin(moon_angle_a) + rnorm(points_per_class, sd = 0.18)

    class_b_x <- 3.2 * (1 - cos(moon_angle_b)) + rnorm(points_per_class, sd = 0.18)
    class_b_y <- -3.2 * sin(moon_angle_b) + 1.1 + rnorm(points_per_class, sd = 0.18)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Circles") {
    outer_angle <- runif(points_per_class, min = 0, max = 2 * pi)
    inner_angle <- runif(points_per_class, min = 0, max = 2 * pi)

    class_a_x <- 4.0 * cos(outer_angle) + rnorm(points_per_class, sd = 0.25)
    class_a_y <- 4.0 * sin(outer_angle) + rnorm(points_per_class, sd = 0.25)

    class_b_x <- 1.9 * cos(inner_angle) + rnorm(points_per_class, sd = 0.18)
    class_b_y <- 1.9 * sin(inner_angle) + rnorm(points_per_class, sd = 0.18)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Titanic passengers") {
    if (!requireNamespace("titanic", quietly = TRUE)) {
      stop("Package 'titanic' is required. Run install.packages('titanic') first.")
    }

    titanic_data <- titanic::titanic_train

    titanic_data <- titanic_data[
      stats::complete.cases(
        titanic_data$Age,
        titanic_data$Fare,
        titanic_data$Survived
      ),
      ,
      drop = FALSE
    ]

    titanic_data$class_label <- ifelse(
      titanic_data$Survived == 1,
      "Class B",
      "Class A"
    )

    class_a_rows <- which(titanic_data$class_label == "Class A")
    class_b_rows <- which(titanic_data$class_label == "Class B")

    selected_class_a <- sample(class_a_rows, size = min(points_per_class, length(class_a_rows)))
    selected_class_b <- sample(class_b_rows, size = min(points_per_class, length(class_b_rows)))

    selected_data <- titanic_data[c(selected_class_a, selected_class_b), ]

    # The visualizer is two-dimensional, so the Titanic preset maps
    # passenger age to x and log fare to y.
    x_values <- selected_data$Age
    y_values <- log1p(selected_data$Fare)
    class_labels <- selected_data$class_label

  } else if (dataset_name == "Diabetes health data") {
    if (!requireNamespace("mlbench", quietly = TRUE)) {
      stop("Package 'mlbench' is required. Run install.packages('mlbench') first.")
    }

    data("PimaIndiansDiabetes2", package = "mlbench")

    diabetes_data <- PimaIndiansDiabetes2

    diabetes_data <- diabetes_data[
      stats::complete.cases(
        diabetes_data$glucose,
        diabetes_data$mass,
        diabetes_data$diabetes
      ),
      ,
      drop = FALSE
    ]

    diabetes_data$class_label <- ifelse(
      diabetes_data$diabetes == "pos",
      "Class B",
      "Class A"
    )

    class_a_rows <- which(diabetes_data$class_label == "Class A")
    class_b_rows <- which(diabetes_data$class_label == "Class B")

    selected_class_a <- sample(class_a_rows, size = min(points_per_class, length(class_a_rows)))
    selected_class_b <- sample(class_b_rows, size = min(points_per_class, length(class_b_rows)))

    selected_data <- diabetes_data[c(selected_class_a, selected_class_b), ]

    # The diabetes preset uses glucose and body mass as the two plot axes.
    x_values <- selected_data$glucose
    y_values <- selected_data$mass
    class_labels <- selected_data$class_label
  }

  create_indexed_dataset(x_values, y_values, class_labels)
}

# convert_uploaded_csv_to_dataset()
# Purpose:
#   Validate and standardize a user-uploaded CSV.
# Inputs:
#   - uploaded_table: data frame read from a CSV file
# Output:
#   A clean two-class dataset with required x, y, and class columns.
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

  # Uploaded labels can use any two names. The app standardizes the first
  # observed class to Class A and the second observed class to Class B.
  standardized_class_labels <- ifelse(
    as.character(cleaned_table$class) == unique_classes[1],
    "Class A",
    "Class B"
  )

  create_indexed_dataset(cleaned_table$x, cleaned_table$y, standardized_class_labels)
}


# build_prediction_grid()
# Purpose:
#   Create the rectangular set of x/y locations where the model is evaluated.
# Inputs:
#   - classification_data: current plot data used to size the grid
#   - grid_points: number of sampled positions along each axis
# Output:
#   A data frame of grid coordinates. Training code adds probabilities later.
build_prediction_grid <- function(classification_data, grid_points = 140) {
  x_range <- range(classification_data$x)
  y_range <- range(classification_data$y)
  x_center <- mean(x_range)
  y_center <- mean(y_range)
  largest_span <- max(diff(x_range), diff(y_range))

  if (largest_span == 0) largest_span <- 2

  # The grid is square so the probability heatmap lines up with the square
  # plot area and covers a padded region around the data.
  half_plot_span <- largest_span * 0.68
  x_limits <- x_center + c(-half_plot_span, half_plot_span)
  y_limits <- y_center + c(-half_plot_span, half_plot_span)

  x_sequence <- seq(x_limits[1], x_limits[2], length.out = grid_points)

  y_sequence <- seq(y_limits[1], y_limits[2], length.out = grid_points)

  expand.grid(
    x = x_sequence,
    y = y_sequence
  )
}

