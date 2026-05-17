# Synthetic preset dataset helpers for the ML Visualizer app.

synthetic_preset_dataset_names <- function() {
  c(
    "Gaussian clusters",
    "Linearly separable",
    "Overlapping classes",
    "Moons",
    "Circles",
    "XOR pattern",
    "Polynomial curve",
    "Noisy nonlinear blobs"
  )
}


is_synthetic_preset_dataset <- function(dataset_name) {
  dataset_name %in% synthetic_preset_dataset_names()
}


# generate_synthetic_preset_dataset()
# Purpose:
#   Create the synthetic Class A / Class B presets used by the visualizer.
generate_synthetic_preset_dataset <- function(dataset_name, points_per_class = 40) {
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

  } else if (dataset_name == "XOR pattern") {
    class_a_split <- c(floor(points_per_class / 2), ceiling(points_per_class / 2))
    class_b_split <- c(floor(points_per_class / 2), ceiling(points_per_class / 2))

    class_a_x <- c(
      rnorm(class_a_split[1], mean = -2.2, sd = 0.45),
      rnorm(class_a_split[2], mean = 2.2, sd = 0.45)
    )
    class_a_y <- c(
      rnorm(class_a_split[1], mean = -2.2, sd = 0.45),
      rnorm(class_a_split[2], mean = 2.2, sd = 0.45)
    )
    class_b_x <- c(
      rnorm(class_b_split[1], mean = -2.2, sd = 0.45),
      rnorm(class_b_split[2], mean = 2.2, sd = 0.45)
    )
    class_b_y <- c(
      rnorm(class_b_split[1], mean = 2.2, sd = 0.45),
      rnorm(class_b_split[2], mean = -2.2, sd = 0.45)
    )

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Polynomial curve") {
    class_a_x <- runif(points_per_class, min = -3.2, max = 3.2)
    class_b_x <- runif(points_per_class, min = -3.2, max = 3.2)

    class_a_curve <- 0.42 * class_a_x^2 - 1.05
    class_b_curve <- 0.42 * class_b_x^2 - 1.05
    class_a_y <- class_a_curve + 1.0 + rnorm(points_per_class, sd = 0.42)
    class_b_y <- class_b_curve - 1.0 + rnorm(points_per_class, sd = 0.42)

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else if (dataset_name == "Noisy nonlinear blobs") {
    class_a_split <- c(
      floor(points_per_class / 3),
      floor(points_per_class / 3),
      points_per_class - 2 * floor(points_per_class / 3)
    )
    class_b_split <- c(
      floor(points_per_class / 3),
      floor(points_per_class / 3),
      points_per_class - 2 * floor(points_per_class / 3)
    )

    class_a_x <- c(
      rnorm(class_a_split[1], mean = -2.6, sd = 0.55),
      rnorm(class_a_split[2], mean = 0.3, sd = 0.65),
      rnorm(class_a_split[3], mean = 2.5, sd = 0.55)
    )
    class_a_y <- c(
      rnorm(class_a_split[1], mean = -1.3, sd = 0.55),
      rnorm(class_a_split[2], mean = 1.6, sd = 0.65),
      rnorm(class_a_split[3], mean = -1.1, sd = 0.55)
    )
    class_b_x <- c(
      rnorm(class_b_split[1], mean = -2.2, sd = 0.65),
      rnorm(class_b_split[2], mean = 0.2, sd = 0.55),
      rnorm(class_b_split[3], mean = 2.5, sd = 0.65)
    )
    class_b_y <- c(
      rnorm(class_b_split[1], mean = 1.4, sd = 0.65),
      rnorm(class_b_split[2], mean = -1.7, sd = 0.55),
      rnorm(class_b_split[3], mean = 1.5, sd = 0.65)
    )

    x_values <- c(class_a_x, class_b_x)
    y_values <- c(class_a_y, class_b_y)
    class_labels <- c(rep("Class A", points_per_class), rep("Class B", points_per_class))

  } else {
    stop("Unknown synthetic preset dataset selected.")
  }

  create_indexed_dataset(x_values, y_values, class_labels)
}
