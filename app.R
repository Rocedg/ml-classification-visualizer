# app.R
# Purpose:
#   Main entry point for the "ML Visualizer" Shiny application.
#   This file loads all module files, defines shared helper functions,
#   creates the top-level navigation, and connects the page modules together.
#
# Functions:
#   - create_indexed_dataset(): Build a clean 2D classification dataset.
#   - generate_preset_dataset(): Create one of the preset demo datasets.
#   - convert_uploaded_csv_to_dataset(): Validate and clean uploaded CSV data.
#   - train_classification_model(): Train the selected classifier.
#   - calculate_classification_metrics(): Compute beginner-friendly metrics.
#
# Inputs / Outputs:
#   Inputs:
#     - Navigation clicks from the top navbar
#     - "Launch Visualizer" click from the home page
#   Outputs:
#     - Full multi-page Shiny application UI
#     - Shared helper functions used by the modules

library(shiny)
library(ggplot2)


# --------------------------- Shared Helper Functions ---------------------------

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


calculate_classification_metrics <- function(actual_labels, predicted_labels) {
  actual_labels <- factor(actual_labels, levels = c("Class A", "Class B"))
  predicted_labels <- factor(predicted_labels, levels = c("Class A", "Class B"))

  true_positive <- sum(actual_labels == "Class B" & predicted_labels == "Class B")
  true_negative <- sum(actual_labels == "Class A" & predicted_labels == "Class A")
  false_positive <- sum(actual_labels == "Class A" & predicted_labels == "Class B")
  false_negative <- sum(actual_labels == "Class B" & predicted_labels == "Class A")

  accuracy <- (true_positive + true_negative) / length(actual_labels)
  precision <- if ((true_positive + false_positive) == 0) 0 else true_positive / (true_positive + false_positive)
  recall <- if ((true_positive + false_negative) == 0) 0 else true_positive / (true_positive + false_negative)
  f1_score <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)

  list(
    accuracy = round(accuracy, 3),
    precision = round(precision, 3),
    recall = round(recall, 3),
    f1_score = round(f1_score, 3)
  )
}


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
  } else if (algorithm_name == "svm") {
    if (!requireNamespace("e1071", quietly = TRUE)) {
      stop("Package 'e1071' is required for SVM. Install it with install.packages('e1071').")
    }

    trained_model <- e1071::svm(
      class ~ x + y,
      data = classification_data,
      type = "C-classification",
      kernel = parameter_values$svm_kernel,
      cost = parameter_values$svm_cost,
      gamma = parameter_values$svm_gamma,
      probability = TRUE,
      scale = TRUE
    )

    training_predictions <- predict(trained_model, newdata = classification_data, probability = TRUE)
    training_probability_table <- attr(training_predictions, "probabilities")

    training_class_b_probability <- if ("Class B" %in% colnames(training_probability_table)) {
      training_probability_table[, "Class B"]
    } else {
      as.numeric(training_predictions == "Class B")
    }

    grid_predictions <- predict(trained_model, newdata = prediction_grid, probability = TRUE)
    grid_probability_table <- attr(grid_predictions, "probabilities")

    grid_class_b_probability <- if ("Class B" %in% colnames(grid_probability_table)) {
      grid_probability_table[, "Class B"]
    } else {
      as.numeric(grid_predictions == "Class B")
    }

    algorithm_label <- "Support Vector Machine"
  } else if (algorithm_name == "knn") {
    if (!requireNamespace("class", quietly = TRUE)) {
      stop("Package 'class' is required for k-NN. Install it with install.packages('class').")
    }

    training_matrix <- as.matrix(classification_data[, c("x", "y")])
    grid_matrix <- as.matrix(prediction_grid[, c("x", "y")])

    training_predictions <- class::knn(
      train = training_matrix,
      test = training_matrix,
      cl = classification_data$class,
      k = parameter_values$knn_neighbors,
      prob = TRUE
    )

    training_vote_share <- attr(training_predictions, "prob")
    training_class_b_probability <- ifelse(training_predictions == "Class B", training_vote_share, 1 - training_vote_share)

    grid_predictions <- class::knn(
      train = training_matrix,
      test = grid_matrix,
      cl = classification_data$class,
      k = parameter_values$knn_neighbors,
      prob = TRUE
    )

    grid_vote_share <- attr(grid_predictions, "prob")
    grid_class_b_probability <- ifelse(grid_predictions == "Class B", grid_vote_share, 1 - grid_vote_share)

    trained_model <- list(k = parameter_values$knn_neighbors)
    algorithm_label <- "k-Nearest Neighbors"
  } else {
    stop("Unknown algorithm selected.")
  }

  if (algorithm_name == "logistic_regression") {
    trained_model <- logistic_training_results$model_object
    prediction_grid <- logistic_training_results$prediction_grid
    training_predictions <- logistic_training_results$training_predictions
    metrics <- logistic_training_results$metrics
    iteration_history <- logistic_training_results$iterations
    iteration_metrics <- logistic_training_results$iteration_metrics
  } else {
    training_predictions <- factor(training_predictions, levels = c("Class A", "Class B"))
    grid_predictions <- factor(grid_predictions, levels = c("Class A", "Class B"))

    prediction_grid$predicted_class <- grid_predictions
    prediction_grid$class_b_probability <- grid_class_b_probability

    metrics <- calculate_classification_metrics(
      actual_labels = classification_data$class,
      predicted_labels = training_predictions
    )

    iteration_history <- NULL
    iteration_metrics <- NULL
  }

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


# Load module files after shared helper functions are defined.
# This makes helper functions like generate_preset_dataset() available
# everywhere before the modules are sourced and used.
module_files <- c(
  "modules/home_module.R",
  "modules/visualizer_module.R",
  "modules/theory_hub_module.R",
  "modules/about_module.R",
  "modules/dataset_controls_module.R",
  "modules/algorithm_controls_module.R",
  "modules/plot_panel_module.R",
  "modules/raw_data_module.R",
  "modules/model_theory_panel_module.R"
)

for (module_file in module_files) {
  sys.source(module_file, envir = environment())
}


# ------------------------------- Application UI -------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("ML Visualizer"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700;800&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(
    class = "page-shell",
    uiOutput("top_navbar"),
    div(
      class = "page-content-shell",
      tabsetPanel(
        id = "main_navigation",
        type = "hidden",
        selected = "home",
        tabPanel("home", home_module_ui("home_page")),
        tabPanel("visualizer", visualizer_module_ui("visualizer_page")),
        tabPanel("theory_hub", theory_hub_module_ui("theory_hub_page")),
        tabPanel("about_us", about_module_ui("about_page"))
      )
    )
  )
)


# ----------------------------- Application Server ----------------------------

server <- function(input, output, session) {
  current_page <- reactiveVal("home")

  output$top_navbar <- renderUI({
    nav_link <- function(link_id, label, page_value) {
      active_class <- if (identical(current_page(), page_value)) "top-nav-link is-active" else "top-nav-link"
      actionLink(inputId = link_id, label = label, class = active_class)
    }

    div(
      class = "top-navbar",
      div(class = "top-navbar-inner",
        div(class = "brand-mark", "ML Visualizer"),
        div(class = "top-navbar-links",
          nav_link("nav_home", "Home", "home"),
          nav_link("nav_visualizer", "The Visualizer", "visualizer"),
          nav_link("nav_theory_hub", "Theory Hub", "theory_hub"),
          nav_link("nav_about_us", "About Us", "about_us")
        )
      )
    )
  })

  home_page <- home_module_server("home_page")
  visualizer_module_server("visualizer_page")
  theory_hub_module_server("theory_hub_page")
  about_module_server("about_page")

  observeEvent(input$nav_home, {
    current_page("home")
  })

  observeEvent(input$nav_visualizer, {
    current_page("visualizer")
  })

  observeEvent(input$nav_theory_hub, {
    current_page("theory_hub")
  })

  observeEvent(input$nav_about_us, {
    current_page("about_us")
  })

  observeEvent(home_page$launch_visualizer(), {
    current_page("visualizer")
  })

  observe({
    updateTabsetPanel(session, inputId = "main_navigation", selected = current_page())
  })
}

shinyApp(ui = ui, server = server)
