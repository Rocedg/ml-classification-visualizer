# Main probability plot helpers for the ML Visualizer app.

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
#   - knn_inspection: optional selected query point and nearest neighbors
# Output:
#   A ggplot object rendered by the plot panel module.
build_classification_plot <- function(classification_data, active_model_view, knn_inspection = NULL) {
  plot_object <- ggplot()
  point_data <- classification_data
  model_algorithm_key <- if (!is.null(active_model_view$algorithm_key)) active_model_view$algorithm_key else NA_character_
  uses_svm_plot <- identical(model_algorithm_key, "svm")

  if (!is.null(active_model_view) && !is.null(active_model_view$classification_data)) {
    point_data <- active_model_view$classification_data
  }

  square_plot_limits <- build_square_plot_limits(point_data$x, point_data$y)
  plot_x_limits <- square_plot_limits$x
  plot_y_limits <- square_plot_limits$y

  if (!is.null(active_model_view)) {
    prediction_grid <- active_model_view$prediction_grid
    plot_x_limits <- range(prediction_grid$x)
    plot_y_limits <- range(prediction_grid$y)

    if (uses_svm_plot) {
      region_grid <- prediction_grid[!is.na(prediction_grid$predicted_class), , drop = FALSE]

      if (nrow(region_grid) > 0) {
        plot_object <- plot_object +
          geom_raster(
            data = region_grid,
            aes(x = x, y = y, fill = predicted_class),
            alpha = 0.42,
            interpolate = FALSE
          ) +
          scale_fill_manual(
            values = c("Class A" = "#d8e8ff", "Class B" = "#ffe4d3"),
            guide = "none"
          )
      }

      if ("decision_value" %in% names(prediction_grid)) {
        finite_decision_values <- prediction_grid$decision_value[
          !is.na(prediction_grid$decision_value) & is.finite(prediction_grid$decision_value)
        ]

        if (length(finite_decision_values) > 0) {
          decision_value_range <- range(finite_decision_values)
          break_is_visible <- function(contour_break) {
            decision_value_range[1] <= contour_break && decision_value_range[2] >= contour_break
          }

          if (break_is_visible(0)) {
            plot_object <- plot_object +
              geom_contour(
                data = prediction_grid,
                aes(x = x, y = y, z = decision_value),
                breaks = 0,
                color = "#111827",
                linewidth = 0.65,
                inherit.aes = FALSE
              )
          }

          score_contour_breaks <- c(-1, 1)[vapply(c(-1, 1), break_is_visible, logical(1))]

          if (length(score_contour_breaks) > 0) {
            plot_object <- plot_object +
              geom_contour(
                data = prediction_grid,
                aes(x = x, y = y, z = decision_value),
                breaks = score_contour_breaks,
                color = "#475569",
                linewidth = 0.5,
                linetype = "dashed",
                alpha = 0.9,
                inherit.aes = FALSE
              )
          }
        }
      }
    } else {
      # Training stores Class B probabilities. The background scale is expressed
      # as Class A probability so blue means more likely Class A and orange means
      # more likely Class B.
      prediction_grid$class_a_probability <- 1 - prediction_grid$class_b_probability

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
  }

  if ("split" %in% names(point_data)) {
    point_data$split <- factor(as.character(point_data$split), levels = c("train", "test"))
    train_point_data <- point_data[point_data$split == "train", , drop = FALSE]
    test_point_data <- point_data[point_data$split == "test", , drop = FALSE]

    point_layers <- list(
      geom_point(
        data = point_data,
        aes(x = x, y = y),
        color = "#ffffff",
        size = 3.8,
        alpha = 0.9
      ),
      geom_point(
        data = train_point_data,
        aes(x = x, y = y, color = class),
        size = 2.6,
        alpha = 0.98,
        shape = 16
      ),
      geom_point(
        data = test_point_data,
        aes(x = x, y = y, color = class),
        fill = "#ffffff",
        size = 3.0,
        alpha = 0.98,
        shape = 21,
        stroke = 1.2
      ),
      scale_color_manual(values = c("Class A" = "#5a95ff", "Class B" = "#ff8b3d"), guide = "none")
    )
  } else {
    point_layers <- list(
      geom_point(
        data = point_data,
        aes(x = x, y = y),
        color = "#ffffff",
        size = 3.4,
        alpha = 0.9
      ),
      geom_point(
        data = point_data,
        aes(x = x, y = y, color = class),
        size = 2.55,
        alpha = 0.98
      ),
      scale_color_manual(values = c("Class A" = "#5a95ff", "Class B" = "#ff8b3d"), guide = "none")
    )
  }

  support_vector_layers <- list()
  if (uses_svm_plot && !is.null(active_model_view$support_vectors) && nrow(active_model_view$support_vectors) > 0) {
    support_vector_layers <- list(
      geom_point(
        data = active_model_view$support_vectors,
        aes(x = x, y = y, color = class),
        fill = NA,
        size = 5.2,
        alpha = 0.98,
        shape = 21,
        stroke = 1.35,
        inherit.aes = FALSE
      )
    )
  }

  inspection_layers <- list()
  if (!is.null(knn_inspection) && !is.null(knn_inspection$query_point)) {
    query_point <- knn_inspection$query_point
    nearest_neighbors <- knn_inspection$neighbors

    if (!is.null(nearest_neighbors) && nrow(nearest_neighbors) > 0) {
      segment_data <- data.frame(
        x = rep(query_point$x[1], nrow(nearest_neighbors)),
        y = rep(query_point$y[1], nrow(nearest_neighbors)),
        xend = nearest_neighbors$x,
        yend = nearest_neighbors$y
      )

      inspection_layers <- c(
        inspection_layers,
        list(
          geom_segment(
            data = segment_data,
            aes(x = x, y = y, xend = xend, yend = yend),
            color = "#111827",
            linewidth = 0.35,
            linetype = "dashed",
            alpha = 0.32,
            inherit.aes = FALSE
          ),
          geom_point(
            data = nearest_neighbors,
            aes(x = x, y = y),
            color = "#111827",
            fill = NA,
            size = 4.6,
            alpha = 0.98,
            shape = 21,
            stroke = 1.35,
            inherit.aes = FALSE
          )
        )
      )
    }

    inspection_layers <- c(
      inspection_layers,
      list(
        geom_point(
          data = query_point,
          aes(x = x, y = y),
          color = "#111827",
          size = 5,
          alpha = 0.98,
          shape = 4,
          stroke = 1.4,
          inherit.aes = FALSE
        )
      )
    )
  }

  plot_object +
    geom_hline(yintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    geom_vline(xintercept = 0, color = "#d6dfe8", linewidth = 0.6) +
    point_layers +
    support_vector_layers +
    inspection_layers +
    coord_equal(xlim = plot_x_limits, ylim = plot_y_limits, expand = FALSE) +
    labs(
      x = "X",
      y = "Y"
    ) +
    theme_minimal(base_family = "Manrope") +
    theme(
      legend.position = "none",
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


