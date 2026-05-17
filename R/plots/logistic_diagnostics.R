# Logistic Regression diagnostic plot helpers for the ML Visualizer app.

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
#   - convergence_epsilon: visual-only threshold for marking tiny loss changes
# Output:
#   A ggplot line chart for the diagnostic area.
build_iteration_metric_plot <- function(metric_history, current_iteration, convergence_epsilon = 0.001) {
  highlighted_iteration <- min(max(current_iteration, 1), nrow(metric_history))
  highlighted_metric <- metric_history[highlighted_iteration, , drop = FALSE]

  convergence_metric <- NULL
  if (nrow(metric_history) >= 2) {
    previous_loss <- metric_history$loss[-nrow(metric_history)]
    current_loss <- metric_history$loss[-1]
    valid_loss_pair <- is.finite(previous_loss) & is.finite(current_loss)
    loss_change <- abs(current_loss - previous_loss)
    convergence_candidates <- which(valid_loss_pair & loss_change < convergence_epsilon)

    if (length(convergence_candidates) > 0) {
      convergence_metric <- metric_history[convergence_candidates[1] + 1, , drop = FALSE]
      convergence_metric$label <- paste0("loss change < ", convergence_epsilon)
    }
  }

  loss_plot <- ggplot(metric_history, aes(x = iteration, y = loss)) +
    geom_line(color = "#5db5a2", linewidth = 1) +
    geom_point(color = "#d7ebe6", size = 1.8) +
    geom_point(
      data = highlighted_metric,
      aes(x = iteration, y = loss),
      color = "#243b57",
      fill = "#79c9b7",
      size = 4.4,
      shape = 21,
      stroke = 1.2,
      inherit.aes = FALSE
    ) +
    labs(
      x = "Iteration",
      y = "Binary Cross-Entropy Loss",
      subtitle = paste("Training accuracy at this step:", highlighted_metric$accuracy)
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

  if (!is.null(convergence_metric)) {
    convergence_label_hjust <- if (convergence_metric$iteration > stats::median(metric_history$iteration)) 1.05 else -0.05

    loss_plot <- loss_plot +
      geom_vline(
        xintercept = convergence_metric$iteration,
        color = "#ff8b3d",
        linewidth = 0.55,
        linetype = "dashed",
        alpha = 0.75
      ) +
      geom_point(
        data = convergence_metric,
        aes(x = iteration, y = loss),
        color = "#7c2d12",
        fill = "#ffb36f",
        size = 3.2,
        shape = 23,
        stroke = 0.8,
        inherit.aes = FALSE
      ) +
      geom_label(
        data = convergence_metric,
        aes(x = iteration, y = loss, label = label),
        hjust = convergence_label_hjust,
        vjust = -0.8,
        color = "#7c2d12",
        fill = "#fff7ed",
        linewidth = 0.2,
        label.padding = grid::unit(3, "pt"),
        size = 3,
        inherit.aes = FALSE
      )
  }

  loss_plot
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
  empty_trajectory_data <- data.frame(
    weight_x = numeric(0),
    weight_y = numeric(0),
    bias = numeric(0)
  )

  empty_plot <- plotly::plot_ly(
    data = empty_trajectory_data,
    x = ~weight_x,
    y = ~weight_y,
    z = ~bias,
    type = "scatter3d",
    mode = "markers",
    hoverinfo = "none",
    showlegend = FALSE
  )

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

  trajectory_plot <- plotly::plot_ly(
    data = trajectory_data,
    x = ~weight_x,
    y = ~weight_y,
    z = ~bias,
    type = "scatter3d",
    mode = "lines+markers",
    name = "Training path",
    text = ~tooltip,
    hoverinfo = "text",
    line = list(color = "#5db5a2", width = 5),
    marker = list(
      size = 3,
      color = trajectory_data$iteration,
      colorscale = list(
        list(0, "#b9ddd5"),
        list(0.5, "#5db5a2"),
        list(1, "#243b57")
      ),
      showscale = TRUE,
      colorbar = list(title = "Iteration")
    )
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
#   Compute plain binary cross-entropy loss for a candidate parameter set.
# Used by:
#   The 2D loss landscape shown when fit_intercept is off.
# Output:
#   One numeric loss value for weight_x, weight_y, and bias.
calculate_logistic_objective <- function(weight_x, weight_y, bias, classification_data) {
  binary_target <- ifelse(classification_data$class == "Class B", 1, 0)
  linear_scores <- bias + weight_x * classification_data$x + weight_y * classification_data$y
  probabilities <- sigmoid_probability(linear_scores)
  safe_probabilities <- pmin(pmax(probabilities, 1e-6), 1 - 1e-6)

  log_loss <- -mean(
    binary_target * log(safe_probabilities) +
      (1 - binary_target) * log(1 - safe_probabilities)
  )

  log_loss
}


# build_bias_fixed_loss_landscape_plot()
# Purpose:
#   Draw a 2D loss surface over weight_x and weight_y when bias is fixed to 0.
# Inputs:
#   - classification_data: current training data
#   - iteration_history: saved logistic states for the parameter path
#   - current_iteration: selected playback index
# Output:
#   A Plotly heatmap/contour showing the loss landscape and parameter path.
build_bias_fixed_loss_landscape_plot <- function(classification_data,
                                                 iteration_history,
                                                 current_iteration) {
  trajectory_data <- build_parameter_trajectory_data(iteration_history)

  build_empty_loss_landscape_plot <- function(message) {
    empty_loss_data <- data.frame(
      weight_x = numeric(0),
      weight_y = numeric(0)
    )

    empty_plot <- plotly::plot_ly(
      data = empty_loss_data,
      x = ~weight_x,
      y = ~weight_y,
      type = "scatter",
      mode = "markers",
      hoverinfo = "none",
      showlegend = FALSE
    )

    plotly::layout(
      empty_plot,
      title = list(text = "Loss landscape without intercept"),
      annotations = list(
        list(
          text = message,
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
      margin = list(l = 0, r = 0, b = 0, t = 60),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
  }

  if (
    is.null(classification_data) ||
      nrow(classification_data) == 0 ||
      !all(c("x", "y", "class") %in% names(classification_data)) ||
      is.null(trajectory_data) ||
      nrow(trajectory_data) == 0
  ) {
    return(
      build_empty_loss_landscape_plot(
        "Run Logistic Regression with Fit intercept off to see the 2D loss landscape."
      )
    )
  }

  trajectory_data <- trajectory_data[
    is.finite(trajectory_data$weight_x) & is.finite(trajectory_data$weight_y),
    ,
    drop = FALSE
  ]

  if (nrow(trajectory_data) == 0) {
    return(
      build_empty_loss_landscape_plot("No finite parameter path is available for the 2D loss landscape.")
    )
  }

  classification_data <- classification_data[
    is.finite(classification_data$x) & is.finite(classification_data$y),
    ,
    drop = FALSE
  ]

  if (nrow(classification_data) == 0) {
    return(
      build_empty_loss_landscape_plot("No finite training points are available for the 2D loss landscape.")
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

  weight_x_values <- seq(x_range[1] - x_padding, x_range[2] + x_padding, length.out = 60)
  weight_y_values <- seq(y_range[1] - y_padding, y_range[2] + y_padding, length.out = 60)

  loss_matrix <- matrix(
    NA_real_,
    nrow = length(weight_y_values),
    ncol = length(weight_x_values)
  )

  for (row_index in seq_along(weight_y_values)) {
    loss_matrix[row_index, ] <- vapply(
      weight_x_values,
      function(candidate_weight_x) {
        calculate_logistic_objective(
          weight_x = candidate_weight_x,
          weight_y = weight_y_values[row_index],
          bias = 0,
          classification_data = classification_data
        )
      },
      numeric(1)
    )
  }

  finite_losses <- loss_matrix[is.finite(loss_matrix)]
  if (length(finite_losses) == 0) {
    finite_losses <- 0
  }
  loss_cap <- as.numeric(stats::quantile(finite_losses, 0.95, na.rm = TRUE))
  if (!is.finite(loss_cap)) {
    loss_cap <- max(finite_losses, na.rm = TRUE)
  }
  loss_display_matrix <- pmin(loss_matrix, loss_cap)

  trajectory_data$loss_value <- vapply(
    seq_len(nrow(trajectory_data)),
    function(path_index) {
      calculate_logistic_objective(
        weight_x = trajectory_data$weight_x[path_index],
        weight_y = trajectory_data$weight_y[path_index],
        bias = 0,
        classification_data = classification_data
      )
    },
    numeric(1)
  )
  trajectory_data$tooltip <- paste0(
    "Iteration: ", trajectory_data$iteration,
    "<br>Weight x: ", round(trajectory_data$weight_x, 4),
    "<br>Weight y: ", round(trajectory_data$weight_y, 4),
    "<br>Loss: ", round(trajectory_data$loss_value, 4)
  )

  marker_data <- rbind(
    data.frame(marker = "Start", trajectory_data[1, c("weight_x", "weight_y", "iteration", "tooltip")]),
    data.frame(marker = "Current", trajectory_data[bounded_iteration, c("weight_x", "weight_y", "iteration", "tooltip")]),
    data.frame(marker = "Final", trajectory_data[nrow(trajectory_data), c("weight_x", "weight_y", "iteration", "tooltip")])
  )
  marker_data$marker <- factor(marker_data$marker, levels = c("Start", "Current", "Final"))

  loss_plot <- plotly::plot_ly(
    x = weight_x_values,
    y = weight_y_values,
    z = loss_display_matrix,
    type = "heatmap",
    colorscale = list(
      list(0, "#e0f2fe"),
      list(0.45, "#d7ebe6"),
      list(0.7, "#fff7ed"),
      list(1, "#fb923c")
    ),
    colorbar = list(title = "Loss"),
    hovertemplate = paste(
      "Weight x: %{x:.3f}",
      "<br>Weight y: %{y:.3f}",
      "<br>Loss: %{z:.4f}",
      "<extra></extra>"
    )
  )

  loss_plot <- plotly::add_trace(
    loss_plot,
    x = weight_x_values,
    y = weight_y_values,
    z = loss_display_matrix,
    type = "contour",
    contours = list(coloring = "none", showlabels = FALSE),
    line = list(color = "rgba(255, 255, 255, 0.65)", width = 1),
    showscale = FALSE,
    hoverinfo = "skip",
    name = "Loss contours",
    inherit = FALSE
  )

  loss_plot <- plotly::add_trace(
    loss_plot,
    data = trajectory_data,
    x = ~weight_x,
    y = ~weight_y,
    type = "scatter",
    mode = "lines+markers",
    name = "Gradient descent path",
    text = ~tooltip,
    hoverinfo = "text",
    line = list(color = "#243b57", width = 3),
    marker = list(
      size = 5,
      color = "#b9ddd5",
      line = list(color = "#243b57", width = 0.5)
    ),
    inherit = FALSE
  )

  marker_styles <- list(
    Start = list(color = "#ffffff", line = list(color = "#243b57", width = 2), symbol = "circle"),
    Current = list(color = "#243b57", line = list(color = "#ffffff", width = 2), symbol = "diamond"),
    Final = list(color = "#ff8b3d", line = list(color = "#7c2d12", width = 2), symbol = "triangle-up")
  )

  for (marker_name in levels(marker_data$marker)) {
    selected_marker <- marker_data[marker_data$marker == marker_name, , drop = FALSE]

    loss_plot <- plotly::add_trace(
      loss_plot,
      data = selected_marker,
      x = ~weight_x,
      y = ~weight_y,
      type = "scatter",
      mode = "markers",
      name = marker_name,
      text = ~paste0(marker, "<br>", tooltip),
      hoverinfo = "text",
      marker = c(list(size = 11), marker_styles[[marker_name]]),
      inherit = FALSE
    )
  }

  plotly::layout(
    loss_plot,
    title = list(
      text = paste0(
        "Loss landscape without intercept",
        "<br><sup>Color shows loss for different weight combinations; the path shows gradient descent updates.",
        "<br>Current iteration: ", trajectory_data$iteration[bounded_iteration], "</sup>"
      )
    ),
    xaxis = list(
      title = "Weight x",
      gridcolor = "#eef3f7",
      zeroline = TRUE,
      zerolinecolor = "#94a3b8"
    ),
    yaxis = list(
      title = "Weight y",
      gridcolor = "#eef3f7",
      zeroline = TRUE,
      zerolinecolor = "#94a3b8"
    ),
    legend = list(orientation = "h", x = 0, y = -0.16),
    margin = list(l = 55, r = 18, b = 50, t = 76),
    paper_bgcolor = "#ffffff",
    plot_bgcolor = "#ffffff"
  )
}
