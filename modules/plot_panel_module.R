# modules/plot_panel_module.R
# Purpose:
#   Render the main interactive plot tab.
#   This module draws:
#   - the input data points
#   - classification regions
#   - the decision boundary
#   - metric summary cards
#   It also exposes plot click coordinates back to the parent module
#   so users can draw their own points.
#
# Functions:
#   - plot_panel_module_ui(): Build the plot tab layout.
#   - plot_panel_module_server(): Render the plot, metric cards,
#     and expose click coordinates.
#
# Inputs / Outputs:
#   Inputs:
#     - Reactive classification dataset
#     - Reactive drawing mode state
#     - Reactive selected drawing class
#     - Reactive count of run button clicks
#   Outputs:
#     - Plot click coordinates
#     - Rendered classification plot
#     - Rendered metric values

plot_panel_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "plot-tab-layout",
    div(
      class = "plot-top-status-row",
      div(class = "status-chip status-chip-primary", textOutput(ns("plot_status_text"), inline = TRUE)),
      div(class = "status-chip", textOutput(ns("drawing_status_text"), inline = TRUE))
    ),
    div(
      class = "plot-canvas-shell",
      plotOutput(
        outputId = ns("classification_plot"),
        height = "620px",
        click = ns("plot_click")
      )
    ),
    div(
      class = "app-card theory-summary-card",
      div(
        class = "plot-top-status-row",
        div(class = "status-chip status-chip-primary", textOutput(ns("iteration_status_text"), inline = TRUE)),
        div(class = "status-chip", textOutput(ns("playback_status_text"), inline = TRUE))
      ),
      div(
        class = "button-row",
        actionButton(
          inputId = ns("play_pause_button"),
          label = "Play",
          class = "ml-button ml-button-primary half-width-button"
        ),
        actionButton(
          inputId = ns("step_backward_button"),
          label = "Step Back",
          class = "ml-button ml-button-secondary half-width-button"
        ),
        actionButton(
          inputId = ns("step_forward_button"),
          label = "Step Forward",
          class = "ml-button ml-button-secondary half-width-button"
        )
      ),
      sliderInput(
        inputId = ns("iteration_slider"),
        label = "Iteration",
        min = 1,
        max = 1,
        value = 1,
        step = 1,
        width = "100%"
      ),
      div(
        class = "run-helper-text",
        textOutput(ns("playback_helper_text"))
      )
    ),
    div(
      class = "plot-canvas-shell",
      plotOutput(
        outputId = ns("iteration_metric_plot"),
        height = "180px"
      )
    ),
    div(
      class = "metric-card-grid",
      div(class = "app-card metric-card",
        tags$span(class = "metric-label", "Accuracy"),
        tags$span(
          class = "metric-value",
          textOutput(ns("accuracy_value"), inline = TRUE)
        )
      ),
      div(class = "app-card metric-card",
        tags$span(class = "metric-label", "Precision"),
        tags$span(
          class = "metric-value",
          textOutput(ns("precision_value"), inline = TRUE)
        )
      ),
      div(class = "app-card metric-card",
        tags$span(class = "metric-label", "Recall"),
        tags$span(
          class = "metric-value",
          textOutput(ns("recall_value"), inline = TRUE)
        )
      ),
      div(class = "app-card metric-card",
        tags$span(class = "metric-label", "F1 Score"),
        tags$span(
          class = "metric-value",
          textOutput(ns("f1_value"), inline = TRUE)
        )
      )
    )
  )
}


plot_panel_module_server <- function(id,
                                     classification_data,
                                     drawing_mode_active,
                                     selected_class_label,
                                     run_model_clicked) {
  moduleServer(id, function(input, output, session) {
    internal_model_reactive <- reactiveVal(NULL)
    current_iteration <- reactiveVal(1)
    playback_running <- reactiveVal(FALSE)

    set_model_reactive <- function(model_reactive_expression) {
      internal_model_reactive(model_reactive_expression)
    }

    safe_model_results <- reactive({
      model_reactive_expression <- internal_model_reactive()

      if (is.null(model_reactive_expression) || run_model_clicked() == 0) {
        return(NULL)
      }

      tryCatch(
        model_reactive_expression(),
        error = function(error_object) {
          NULL
        }
      )
    })

    # TODO: Future: generalize this playback system for other classifiers.
    logistic_iteration_history <- reactive({
      model_results <- safe_model_results()

      if (is.null(model_results) || model_results$algorithm_key != "logistic_regression") {
        return(NULL)
      }

      model_results$iterations
    })

    total_iteration_count <- reactive({
      iteration_history <- logistic_iteration_history()

      if (is.null(iteration_history)) {
        return(0)
      }

      length(iteration_history)
    })

    active_iteration_results <- reactive({
      model_results <- safe_model_results()

      if (is.null(model_results)) {
        return(NULL)
      }

      if (model_results$algorithm_key != "logistic_regression") {
        return(model_results)
      }

      iteration_history <- logistic_iteration_history()
      if (is.null(iteration_history) || length(iteration_history) == 0) {
        return(model_results)
      }

      bounded_iteration <- min(max(current_iteration(), 1), length(iteration_history))
      iteration_history[[bounded_iteration]]
    })

    observeEvent(safe_model_results(), {
      model_results <- safe_model_results()

      playback_running(FALSE)
      current_iteration(1)

      if (!is.null(model_results) && model_results$algorithm_key == "logistic_regression") {
        updateSliderInput(
          session = session,
          inputId = "iteration_slider",
          min = 1,
          max = length(model_results$iterations),
          value = 1
        )
      } else {
        updateSliderInput(
          session = session,
          inputId = "iteration_slider",
          min = 1,
          max = 1,
          value = 1
        )
      }
    }, ignoreInit = TRUE)

    observeEvent(input$iteration_slider, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      current_iteration(input$iteration_slider)
    }, ignoreInit = TRUE)

    observeEvent(input$play_pause_button, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      playback_running(!playback_running())
    })

    observeEvent(input$step_forward_button, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      playback_running(FALSE)
      current_iteration(min(current_iteration() + 1, total_iteration_count()))
    })

    observeEvent(input$step_backward_button, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      playback_running(FALSE)
      current_iteration(max(current_iteration() - 1, 1))
    })

    observe({
      if (!playback_running()) {
        return(NULL)
      }

      total_iterations <- total_iteration_count()
      if (total_iterations == 0) {
        playback_running(FALSE)
        return(NULL)
      }

      invalidateLater(350, session)

      if (current_iteration() >= total_iterations) {
        playback_running(FALSE)
      } else {
        current_iteration(current_iteration() + 1)
      }
    })

    observe({
      updateSliderInput(
        session = session,
        inputId = "iteration_slider",
        value = current_iteration()
      )
    })

    output$plot_status_text <- renderText({
      model_results <- safe_model_results()

      if (is.null(model_results)) {
        "Interactive Plot"
      } else {
        paste("Showing", model_results$algorithm_label)
      }
    })

    output$iteration_status_text <- renderText({
      model_results <- safe_model_results()

      if (is.null(model_results) || model_results$algorithm_key != "logistic_regression") {
        return("Iteration playback ready after Logistic Regression")
      }

      paste("Iteration:", current_iteration(), "/", total_iteration_count())
    })

    output$playback_status_text <- renderText({
      model_results <- safe_model_results()

      if (is.null(model_results) || model_results$algorithm_key != "logistic_regression") {
        return("Playback inactive")
      }

      if (playback_running()) {
        "Playback running"
      } else {
        "Playback paused"
      }
    })

    output$playback_helper_text <- renderText({
      model_results <- safe_model_results()

      if (is.null(model_results)) {
        return("Run Logistic Regression to unlock the step-by-step training replay controls.")
      }

      if (model_results$algorithm_key != "logistic_regression") {
        return("Iteration playback is currently available only for Logistic Regression.")
      }

      active_iteration <- active_iteration_results()

      paste(
        "Loss:",
        active_iteration$loss_value,
        "| Use the slider or buttons to move through how the boundary changes over time."
      )
    })

    output$drawing_status_text <- renderText({
      if (drawing_mode_active()) {
        paste("Drawing", selected_class_label(), "points")
      } else {
        "Drawing mode off"
      }
    })

    output$classification_plot <- renderPlot({
      current_classification_data <- classification_data()
      active_model_view <- active_iteration_results()

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
          data = current_classification_data,
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
    }, res = 110)

    output$iteration_metric_plot <- renderPlot({
      model_results <- safe_model_results()

      if (is.null(model_results) || model_results$algorithm_key != "logistic_regression") {
        plot.new()
        text(
          x = 0.5,
          y = 0.5,
          labels = "Loss over time will appear here after you run Logistic Regression.",
          col = "#6d8196",
          cex = 1
        )
        return(invisible(NULL))
      }

      metric_history <- model_results$iteration_metrics
      highlighted_iteration <- min(max(current_iteration(), 1), nrow(metric_history))
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
    }, res = 110)

    output$accuracy_value <- renderText({
      active_model_view <- active_iteration_results()
      if (is.null(active_model_view)) return("Run model")
      active_model_view$metrics$accuracy
    })

    output$precision_value <- renderText({
      active_model_view <- active_iteration_results()
      if (is.null(active_model_view)) return("Run model")
      active_model_view$metrics$precision
    })

    output$recall_value <- renderText({
      active_model_view <- active_iteration_results()
      if (is.null(active_model_view)) return("Run model")
      active_model_view$metrics$recall
    })

    output$f1_value <- renderText({
      active_model_view <- active_iteration_results()
      if (is.null(active_model_view)) return("Run model")
      active_model_view$metrics$f1_score
    })

    observe({
      if (playback_running()) {
        updateActionButton(session, "play_pause_button", label = "Pause")
      } else {
        updateActionButton(session, "play_pause_button", label = "Play")
      }
    })

    list(
      plot_click_coordinates = reactive(input$plot_click),
      set_model_reactive = set_model_reactive
    )
  })
}
