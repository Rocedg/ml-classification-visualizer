# Plot helper functions for the ML Visualizer app.

# build_square_plot_limits()
# Purpose:
#   Compute equal-width x and y ranges so scatter plots render as squares.
# Inputs:
#   - x_values, y_values: coordinates that should fit inside the plot
#   - padding_fraction: extra space around the largest data span
# Output:
#   A list with x and y limits for coord_equal().
build_square_plot_limits <- function(x_values, y_values, padding_fraction = 0.18) {
  x_range <- range(x_values)
  y_range <- range(y_values)
  x_center <- mean(x_range)
  y_center <- mean(y_range)
  largest_span <- max(diff(x_range), diff(y_range))

  if (largest_span == 0) largest_span <- 2

  half_plot_span <- largest_span * (0.5 + padding_fraction)

  list(
    x = x_center + c(-half_plot_span, half_plot_span),
    y = y_center + c(-half_plot_span, half_plot_span)
  )
}


# build_classification_plot()
# Purpose:
#   Draw the main classifier plot: axes, optional probability heatmap, and data.
# Inputs:
#   - classification_data: current points from preset/upload/drawing workflows
#   - active_model_view: NULL before training, or one saved model iteration
# Output:
#   A ggplot object rendered by the plot panel module.
build_classification_plot <- function(classification_data, active_model_view) {
  plot_object <- ggplot()
  square_plot_limits <- build_square_plot_limits(classification_data$x, classification_data$y)
  plot_x_limits <- square_plot_limits$x
  plot_y_limits <- square_plot_limits$y

  if (!is.null(active_model_view)) {
    prediction_grid <- active_model_view$prediction_grid
    # Training stores Class B probabilities. The background scale is expressed
    # as Class A probability so blue means more likely Class A and orange means
    # more likely Class B.
    prediction_grid$class_a_probability <- 1 - prediction_grid$class_b_probability
    plot_x_limits <- range(prediction_grid$x)
    plot_y_limits <- range(prediction_grid$y)

    plot_object <- plot_object +
      # Each grid cell becomes one heatmap tile. Rounding to 0.05 steps keeps
      # the probability field visually readable while still showing transitions.
      geom_raster(
        data = prediction_grid,
        aes(x = x, y = y, fill = round(class_a_probability / 0.05) * 0.05),
        alpha = 0.94,
        interpolate = FALSE
      ) +
      scale_fill_gradientn(
        colours = c("#ff9b6b", "#ffe4d3", "#fff7ed", "#d8e8ff", "#74a9ff"),
        values = c(0, 0.25, 0.5, 0.75, 1),
        limits = c(0, 1),
        breaks = c(1, 0.75, 0.5, 0.25, 0),
        labels = c(
          "Class A probability = 1.0\n(100%)",
          "More likely Class A",
          "0.5 = uncertain\nDecision threshold",
          "More likely Class B",
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


# draw_empty_iteration_metric_plot()
# Purpose:
#   Show a placeholder where the loss-over-time chart will appear.
# Output:
#   A ggplot placeholder used before Logistic Regression has been trained.
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


# build_iteration_metric_plot()
# Purpose:
#   Plot loss across saved training iterations and highlight the active step.
# Inputs:
#   - metric_history: per-iteration loss and accuracy from training
#   - current_iteration: selected playback index from the plot panel
# Output:
#   A ggplot line chart for the diagnostic area.
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


# build_parameter_trajectory_data()
# Purpose:
#   Convert saved logistic iterations into one data frame of parameters.
# Input:
#   - iteration_history: list of saved model states
# Output:
#   Data frame with iteration, weight_x, weight_y, and bias columns.
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


# build_empty_parameter_trajectory_3d_plot()
# Purpose:
#   Provide a placeholder Plotly object before a 3D trajectory can be shown.
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


# build_parameter_trajectory_3d_plot()
# Purpose:
#   Visualize how weight_x, weight_y, and bias move during training.
# Inputs:
#   - iteration_history: saved logistic states from the model result
#   - current_iteration: active playback index to emphasize
# Output:
#   A Plotly 3D path with Start, Current, and Final parameter markers.
build_parameter_trajectory_3d_plot <- function(iteration_history, current_iteration) {
  trajectory_data <- build_parameter_trajectory_data(iteration_history)

  if (is.null(trajectory_data) || nrow(trajectory_data) == 0) {
    return(build_empty_parameter_trajectory_3d_plot())
  }

  if (is.null(current_iteration) || !is.numeric(current_iteration) || length(current_iteration) != 1 || is.na(current_iteration)) {
    current_iteration <- 1
  }
  # Playback indexes are bounded so the plot always points at a stored state.
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


# calculate_logistic_objective()
# Purpose:
#   Compute the regularized logistic loss for a candidate parameter set.
# Used by:
#   The 2D loss landscape shown when fit_intercept is off.
# Output:
#   One numeric loss value for weight_x, weight_y, and bias.
calculate_logistic_objective <- function(weight_x, weight_y, bias, classification_data, model_object = NULL) {
  binary_target <- ifelse(classification_data$class == "Class B", 1, 0)
  linear_scores <- bias + weight_x * classification_data$x + weight_y * classification_data$y
  probabilities <- sigmoid_probability(linear_scores)
  safe_probabilities <- pmin(pmax(probabilities, 1e-6), 1 - 1e-6)

  log_loss <- -mean(
    binary_target * log(safe_probabilities) +
      (1 - binary_target) * log(1 - safe_probabilities)
  )

  # Match the same regularization settings used during training when available.
  regularization_c <- 1
  l1_ratio <- 0

  if (!is.null(model_object$regularization_c)) {
    regularization_c <- model_object$regularization_c
  }
  if (!is.null(model_object$l1_ratio)) {
    l1_ratio <- model_object$l1_ratio
  }

  regularization_strength <- 1 / max(regularization_c, 0.001)
  l1_ratio <- min(max(l1_ratio, 0), 1)
  l2_penalty <- 0.5 * (1 - l1_ratio) * regularization_strength * (weight_x^2 + weight_y^2)
  l1_penalty <- l1_ratio * regularization_strength * (abs(weight_x) + abs(weight_y))

  log_loss + l2_penalty + l1_penalty
}


# build_bias_fixed_loss_landscape_plot()
# Purpose:
#   Draw a 2D loss surface over weight_x and weight_y when bias is fixed to 0.
# Inputs:
#   - classification_data: current training data
#   - iteration_history: saved logistic states for the parameter path
#   - current_iteration: selected playback index
#   - model_object: final model settings, including regularization controls
# Output:
#   A ggplot showing the loss landscape and parameter trajectory.
build_bias_fixed_loss_landscape_plot <- function(classification_data,
                                                 iteration_history,
                                                 current_iteration,
                                                 model_object = NULL) {
  trajectory_data <- build_parameter_trajectory_data(iteration_history)

  if (is.null(trajectory_data) || nrow(trajectory_data) == 0) {
    placeholder_data <- data.frame(
      x = 1,
      y = 1,
      label = "Run Logistic Regression with Fit intercept off to see the 2D loss surface."
    )

    return(
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
    )
  }

  if (is.null(current_iteration) || !is.numeric(current_iteration) || length(current_iteration) != 1 || is.na(current_iteration)) {
    current_iteration <- 1
  }
  bounded_iteration <- min(max(current_iteration, 1), nrow(trajectory_data))

  # The landscape is sampled around the observed training path so the visible
  # surface focuses on the area the optimizer actually traveled through.
  x_range <- range(trajectory_data$weight_x)
  y_range <- range(trajectory_data$weight_y)
  x_padding <- max(diff(x_range) * 0.45, 0.35)
  y_padding <- max(diff(y_range) * 0.45, 0.35)

  weight_grid <- expand.grid(
    weight_x = seq(x_range[1] - x_padding, x_range[2] + x_padding, length.out = 70),
    weight_y = seq(y_range[1] - y_padding, y_range[2] + y_padding, length.out = 70)
  )

  weight_grid$loss_value <- vapply(
    seq_len(nrow(weight_grid)),
    function(row_index) {
      calculate_logistic_objective(
        weight_x = weight_grid$weight_x[row_index],
        weight_y = weight_grid$weight_y[row_index],
        bias = 0,
        classification_data = classification_data,
        model_object = model_object
      )
    },
    numeric(1)
  )
  # Very large losses are capped for display so the color scale keeps useful
  # contrast near the training path.
  weight_grid$loss_display <- pmin(weight_grid$loss_value, stats::quantile(weight_grid$loss_value, 0.95))

  marker_data <- rbind(
    data.frame(marker = "Start", trajectory_data[1, c("weight_x", "weight_y", "iteration")]),
    data.frame(marker = "Current", trajectory_data[bounded_iteration, c("weight_x", "weight_y", "iteration")]),
    data.frame(marker = "Final", trajectory_data[nrow(trajectory_data), c("weight_x", "weight_y", "iteration")])
  )
  marker_data$marker <- factor(marker_data$marker, levels = c("Start", "Current", "Final"))

  ggplot(weight_grid, aes(x = weight_x, y = weight_y)) +
    geom_raster(aes(fill = loss_display), interpolate = TRUE, alpha = 0.95) +
    geom_contour(aes(z = loss_value), color = "#ffffff", linewidth = 0.35, alpha = 0.5, bins = 8) +
    geom_path(
      data = trajectory_data,
      aes(x = weight_x, y = weight_y),
      color = "#243b57",
      linewidth = 0.95,
      alpha = 0.9
    ) +
    geom_point(
      data = trajectory_data,
      aes(x = weight_x, y = weight_y),
      color = "#b9ddd5",
      size = 1.8,
      alpha = 0.9
    ) +
    geom_point(
      data = marker_data,
      aes(x = weight_x, y = weight_y, color = marker, shape = marker),
      size = 4,
      stroke = 0.9
    ) +
    scale_fill_gradientn(
      colours = c("#e0f2fe", "#d7ebe6", "#fff7ed", "#fb923c"),
      name = "Regularized loss"
    ) +
    scale_color_manual(values = c("Start" = "#243b57", "Current" = "#111827", "Final" = "#ff8b3d"), name = NULL) +
    scale_shape_manual(values = c("Start" = 21, "Current" = 23, "Final" = 24), name = NULL) +
    labs(
      title = "Loss landscape (bias fixed to 0)",
      subtitle = paste("Current iteration:", trajectory_data$iteration[bounded_iteration]),
      x = "weight_x",
      y = "weight_y"
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(color = "#334155", size = 9, face = "bold"),
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
