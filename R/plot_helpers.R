# Plot helper functions for the ML Visualizer app.

build_classification_plot <- function(classification_data, active_model_view) {
  plot_object <- ggplot()

  if (!is.null(active_model_view)) {
    prediction_grid <- active_model_view$prediction_grid

    plot_object <- plot_object +
      geom_raster(
        data = prediction_grid,
        aes(x = x, y = y, fill = predicted_class),
        alpha = 0.22
      ) +
      geom_contour(
        data = prediction_grid,
        aes(x = x, y = y, z = class_b_probability),
        breaks = 0.5,
        color = "#1f3552",
        linewidth = 0.65,
        alpha = 0.9
      )
  }

  plot_object +
    geom_hline(yintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    geom_vline(xintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    geom_point(
      data = classification_data,
      aes(x = x, y = y, color = class),
      size = 2.4,
      alpha = 0.95
    ) +
    scale_color_manual(values = c("Class A" = "#5a95ff", "Class B" = "#ff8b3d")) +
    scale_fill_manual(values = c("Class A" = "#dce9ff", "Class B" = "#ffe3d1")) +
    coord_equal() +
    labs(
      x = "X",
      y = "Y"
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eef3f7", linewidth = 0.55),
      axis.title = element_text(color = "#47627b", size = 10, face = "bold"),
      axis.text = element_text(color = "#6d8196", size = 9),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}


draw_empty_iteration_metric_plot <- function() {
  plot.new()
  text(
    x = 0.5,
    y = 0.5,
    labels = "Loss over time will appear here after you run Logistic Regression.",
    col = "#6d8196",
    cex = 1
  )

  invisible(NULL)
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
      y = "Loss",
      subtitle = paste("Accuracy at this step:", highlighted_metric$accuracy)
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eef3f7", linewidth = 0.55),
      axis.title = element_text(color = "#47627b", size = 10, face = "bold"),
      axis.text = element_text(color = "#6d8196", size = 9),
      plot.subtitle = element_text(color = "#6d8196", size = 10),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
}
