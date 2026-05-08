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


build_empty_parameter_trajectory_3d_plot <- function() {
  empty_plot <- plotly::plot_ly()

  plotly::layout(
    empty_plot,
    title = list(text = "Gradient movement in 3D parameter space"),
    annotations = list(
      list(
        text = "Run Logistic Regression to see the optimizer move through weight_x, weight_y, and bias.",
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(color = "#6d8196", size = 14)
      )
    ),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    margin = list(l = 0, r = 0, b = 0, t = 50)
  )
}


build_parameter_trajectory_3d_plot <- function(iteration_history, current_iteration) {
  trajectory_data <- build_parameter_trajectory_data(iteration_history)

  if (is.null(trajectory_data) || nrow(trajectory_data) == 0) {
    return(build_empty_parameter_trajectory_3d_plot())
  }

  if (is.null(current_iteration) || !is.numeric(current_iteration) || length(current_iteration) != 1 || is.na(current_iteration)) {
    current_iteration <- 1
  }
  bounded_iteration <- min(max(current_iteration, 1), nrow(trajectory_data))

  trajectory_data$tooltip <- paste0(
    "Iteration: ", trajectory_data$iteration,
    "<br>weight_x: ", round(trajectory_data$weight_x, 4),
    "<br>weight_y: ", round(trajectory_data$weight_y, 4),
    "<br>bias: ", round(trajectory_data$bias, 4)
  )

  marker_data <- rbind(
    data.frame(marker = "Start", trajectory_data[1, c("weight_x", "weight_y", "bias", "iteration", "tooltip")]),
    data.frame(marker = "Current", trajectory_data[bounded_iteration, c("weight_x", "weight_y", "bias", "iteration", "tooltip")]),
    data.frame(marker = "Final", trajectory_data[nrow(trajectory_data), c("weight_x", "weight_y", "bias", "iteration", "tooltip")])
  )
  marker_data$marker <- factor(marker_data$marker, levels = c("Start", "Current", "Final"))

  trajectory_plot <- plotly::plot_ly()
  trajectory_plot <- plotly::add_trace(
    trajectory_plot,
    data = trajectory_data,
    x = ~weight_x,
    y = ~weight_y,
    z = ~bias,
    type = "scatter3d",
    mode = "lines+markers",
    name = "Training path",
    text = ~tooltip,
    hoverinfo = "text",
    color = ~iteration,
    colors = c("#b9ddd5", "#5db5a2", "#243b57"),
    line = list(color = "#5db5a2", width = 5),
    marker = list(size = 3, colorbar = list(title = "Iteration"))
  )

  marker_styles <- list(
    Start = list(color = "#ffffff", line = list(color = "#243b57", width = 3), symbol = "circle"),
    Current = list(color = "#243b57", line = list(color = "#ffffff", width = 2), symbol = "diamond"),
    Final = list(color = "#ff8b3d", line = list(color = "#7c2d12", width = 2), symbol = "circle")
  )

  for (marker_name in levels(marker_data$marker)) {
    selected_marker <- marker_data[marker_data$marker == marker_name, , drop = FALSE]

    trajectory_plot <- plotly::add_trace(
      trajectory_plot,
      data = selected_marker,
      x = ~weight_x,
      y = ~weight_y,
      z = ~bias,
      type = "scatter3d",
      mode = "markers",
      name = marker_name,
      text = ~paste0(marker, "<br>", tooltip),
      hoverinfo = "text",
      marker = c(list(size = 7), marker_styles[[marker_name]]),
      inherit = FALSE
    )
  }

  plotly::layout(
    trajectory_plot,
    title = list(
      text = paste0(
        "Gradient movement in 3D parameter space",
        "<br><sup>Current iteration: ", trajectory_data$iteration[bounded_iteration], "</sup>"
      )
    ),
    scene = list(
      xaxis = list(title = "weight_x", backgroundcolor = "#f8fafc", gridcolor = "#e2e8f0", zerolinecolor = "#94a3b8"),
      yaxis = list(title = "weight_y", backgroundcolor = "#f8fafc", gridcolor = "#e2e8f0", zerolinecolor = "#94a3b8"),
      zaxis = list(title = "bias", backgroundcolor = "#f8fafc", gridcolor = "#e2e8f0", zerolinecolor = "#94a3b8"),
      camera = list(eye = list(x = 1.45, y = 1.45, z = 1.1))
    ),
    legend = list(orientation = "h", x = 0, y = -0.08),
    margin = list(l = 0, r = 0, b = 0, t = 60),
    paper_bgcolor = "#ffffff",
    plot_bgcolor = "#ffffff"
  )
}
