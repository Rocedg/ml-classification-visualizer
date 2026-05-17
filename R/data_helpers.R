# Core data helpers for the ML Visualizer app.

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


# build_prediction_grid()
# Purpose:
#   Create the squared set of x/y locations where the model is evaluated.
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
