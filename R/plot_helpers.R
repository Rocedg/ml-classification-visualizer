# Plot helper functions for the ML Visualizer app.

build_classification_plot <- function(classification_data, active_model_view) {
  plot_object <- ggplot()
  plot_x_limits <- NULL
  plot_y_limits <- NULL

  if (!is.null(active_model_view)) {
    prediction_grid <- active_model_view$prediction_grid
    prediction_grid$class_a_probability <- 1 - prediction_grid$class_b_probability
    plot_x_limits <- range(prediction_grid$x)
    plot_y_limits <- range(prediction_grid$y)

    plot_object <- plot_object +
      geom_raster(
        data = prediction_grid,
        aes(x = x, y = y, fill = class_a_probability),
        alpha = 0.9,
        interpolate = TRUE
      ) +
      scale_fill_gradientn(
        colours = c("#ffc5a3", "#fff7ed", "#bdd8ff"),
        values = c(0, 0.5, 1),
        limits = c(0, 1),
        breaks = c(1, 0.5, 0),
        labels = c(
          "Class A probability = 1.0\n(100%)",
          "0.5 = uncertain\nDecision threshold",
          "Class B probability = 1.0\n(100%)"
        ),
        name = "Probability guide\nHigh = Class A\nLow = Class B",
        guide = guide_colorbar(
          title.position = "top",
          barheight = grid::unit(96, "pt"),
          barwidth = grid::unit(12, "pt"),
          ticks = TRUE,
          ticks.colour = "#475569",
          frame.colour = "#cbd5e1"
        )
      )
  }

  plot_object +
    geom_hline(yintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    geom_vline(xintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    geom_point(
      data = classification_data,
      aes(x = x, y = y),
      color = "#ffffff",
      size = 3.4,
      alpha = 0.9
    ) +
    geom_point(
      data = classification_data,
      aes(x = x, y = y, color = class),
      size = 2.55,
      alpha = 0.98
    ) +
    scale_color_manual(values = c("Class A" = "#5a95ff", "Class B" = "#ff8b3d"), guide = "none") +
    coord_equal(xlim = plot_x_limits, ylim = plot_y_limits, expand = FALSE) +
    labs(
      x = "X",
      y = "Y"
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      legend.position = if (is.null(active_model_view)) "none" else "right",
      legend.title = element_text(color = "#334155", size = 9, face = "bold", lineheight = 0.95),
      legend.text = element_text(color = "#475569", size = 8.5, lineheight = 0.95),
      legend.key.height = grid::unit(24, "pt"),
      legend.margin = margin(0, 0, 0, 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eef3f7", linewidth = 0.55),
      axis.title = element_text(color = "#47627b", size = 10, face = "bold"),
      axis.text = element_text(color = "#6d8196", size = 9),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}


draw_empty_iteration_metric_plot <- function() {
  placeholder_data <- data.frame(
    x = 1,
    y = 1,
    label = "Loss over time will appear here after you run Logistic Regression."
  )

  ggplot(placeholder_data, aes(x = x, y = y, label = label)) +
    geom_text(color = "#6d8196", size = 4) +
    xlim(0, 2) +
    ylim(0, 2) +
    theme_void(base_family = "Manrope") +
    theme(
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}


build_iteration_metric_plot <- function(metric_history, current_iteration) {
  highlighted_iteration <- min(max(current_iteration, 1), nrow(metric_history))
  highlighted_metric <- metric_history[highlighted_iteration, , drop = FALSE]

  ggplot(metric_history, aes(x = iteration, y = loss)) +
    geom_line(color = "#5db5a2", linewidth = 1) +
    geom_point(color = "#d7ebe6", size = 1.8) +
    geom_point(
      data = highlighted_metric,
      color = "#243b57",
      fill = "#79c9b7",
      size = 3,
      shape = 21,
      stroke = 0.8
    ) +
    labs(
      x = "Iteration",
      y = "Regularized Loss",
      subtitle = paste("Accuracy at this step:", highlighted_metric$accuracy)
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eef3f7", linewidth = 0.55),
      axis.title = element_text(color = "#47627b", size = 10, face = "bold"),
      axis.text = element_text(color = "#6d8196", size = 9),
      plot.subtitle = element_text(color = "#6d8196", size = 10),
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}


draw_empty_parameter_trajectory_plot <- function() {
  placeholder_data <- data.frame(
    x = 1,
    y = 1,
    label = "Parameter movement will appear here after you run Logistic Regression."
  )

  ggplot(placeholder_data, aes(x = x, y = y, label = label)) +
    geom_text(color = "#6d8196", size = 4) +
    xlim(0, 2) +
    ylim(0, 2) +
    theme_void(base_family = "Manrope") +
    theme(
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}


build_parameter_trajectory_data <- function(iteration_history) {
  if (is.null(iteration_history) || length(iteration_history) == 0) {
    return(NULL)
  }

  do.call(
    rbind,
    lapply(seq_along(iteration_history), function(history_index) {
      iteration_step <- iteration_history[[history_index]]
      iteration_number <- iteration_step$iteration_index

      if (is.null(iteration_number)) {
        iteration_number <- history_index
      }

      data.frame(
        iteration = iteration_number,
        weight_x = iteration_step$weight_x,
        weight_y = iteration_step$weight_y,
        bias = iteration_step$bias
      )
    })
  )
}


build_parameter_trajectory_plot <- function(iteration_history, current_iteration, selected_projection) {
  trajectory_data <- build_parameter_trajectory_data(iteration_history)

  if (is.null(trajectory_data) || nrow(trajectory_data) == 0) {
    return(draw_empty_parameter_trajectory_plot())
  }

  projection_options <- list(
    weight_x_weight_y = list(
      x_column = "weight_x",
      y_column = "weight_y",
      x_label = "weight_x",
      y_label = "weight_y"
    ),
    weight_x_bias = list(
      x_column = "weight_x",
      y_column = "bias",
      x_label = "weight_x",
      y_label = "bias"
    ),
    weight_y_bias = list(
      x_column = "weight_y",
      y_column = "bias",
      x_label = "weight_y",
      y_label = "bias"
    )
  )

  if (is.null(selected_projection) || is.null(projection_options[[selected_projection]])) {
    selected_projection <- "weight_x_weight_y"
  }

  selected_option <- projection_options[[selected_projection]]
  if (is.null(current_iteration) || !is.numeric(current_iteration) || length(current_iteration) != 1 || is.na(current_iteration)) {
    current_iteration <- 1
  }
  bounded_iteration <- min(max(current_iteration, 1), nrow(trajectory_data))

  trajectory_plot_data <- trajectory_data
  trajectory_plot_data$x_value <- trajectory_data[[selected_option$x_column]]
  trajectory_plot_data$y_value <- trajectory_data[[selected_option$y_column]]

  marker_data <- rbind(
    data.frame(marker = "Start", trajectory_plot_data[1, c("x_value", "y_value", "iteration")]),
    data.frame(marker = "Current", trajectory_plot_data[bounded_iteration, c("x_value", "y_value", "iteration")]),
    data.frame(marker = "Final", trajectory_plot_data[nrow(trajectory_plot_data), c("x_value", "y_value", "iteration")])
  )
  marker_data$marker <- factor(marker_data$marker, levels = c("Start", "Current", "Final"))

  ggplot(trajectory_plot_data, aes(x = x_value, y = y_value)) +
    geom_path(color = "#5db5a2", linewidth = 0.95, alpha = 0.9) +
    geom_point(color = "#b9ddd5", size = 1.8, alpha = 0.85) +
    geom_point(
      data = marker_data,
      aes(fill = marker, shape = marker),
      color = "#243b57",
      size = 4,
      stroke = 0.9
    ) +
    scale_fill_manual(
      values = c("Start" = "#ffffff", "Current" = "#243b57", "Final" = "#ff8b3d"),
      name = NULL
    ) +
    scale_shape_manual(
      values = c("Start" = 21, "Current" = 23, "Final" = 24),
      name = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = 0.08, add = 0.02)) +
    scale_y_continuous(expand = expansion(mult = 0.08, add = 0.02)) +
    labs(
      title = "Parameter trajectory during training",
      subtitle = paste("Current iteration:", trajectory_plot_data$iteration[bounded_iteration]),
      x = selected_option$x_label,
      y = selected_option$y_label
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      legend.position = "bottom",
      legend.text = element_text(color = "#475569", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eef3f7", linewidth = 0.55),
      axis.title = element_text(color = "#47627b", size = 10, face = "bold"),
      axis.text = element_text(color = "#6d8196", size = 9),
      plot.title = element_text(color = "#243b57", size = 12, face = "bold"),
      plot.subtitle = element_text(color = "#6d8196", size = 10),
      plot.margin = margin(4, 4, 4, 4),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}
