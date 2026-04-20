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

    set_model_reactive <- function(model_reactive_expression) {
      internal_model_reactive(model_reactive_expression)
    }

    safe_model_results <- reactive({
      model_reactive_expression <- internal_model_reactive()

      if (is.null(model_reactive_expression) || run_model_clicked() == 0) {
        return(NULL)
      }

      model_reactive_expression()
    })

    # TODO: Future: generalize this iteration navigation system for other classifiers.
    logistic_iteration_history <- reactive({
      model_results <- safe_model_results()
      get_logistic_iteration_history(model_results)
    })

    total_iteration_count <- reactive({
      iteration_history <- logistic_iteration_history()
      get_iteration_count(iteration_history)
    })

    active_iteration_results <- reactive({
      model_results <- safe_model_results()
      iteration_history <- logistic_iteration_history()
      get_active_iteration_results(model_results, iteration_history, current_iteration())
    })

    observeEvent(safe_model_results(), {
      model_results <- safe_model_results()

      current_iteration(1)

      if (model_supports_logistic_playback(model_results)) {
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
      total_iterations <- total_iteration_count()
      if (total_iterations == 0 || is.null(input$iteration_slider)) {
        return(NULL)
      }

      bounded_iteration <- bound_iteration_index(input$iteration_slider, total_iterations)
      if (!identical(as.integer(current_iteration()), as.integer(bounded_iteration))) {
        current_iteration(bounded_iteration)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$step_forward_button, {
      total_iterations <- total_iteration_count()
      if (total_iterations == 0) {
        return(NULL)
      }

      current_iteration(min(current_iteration() + 1, total_iterations))
    })

    observeEvent(input$step_backward_button, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      current_iteration(max(current_iteration() - 1, 1))
    })

    observe({
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      slider_value <- input$iteration_slider
      iteration_value <- current_iteration()

      if (!is.null(slider_value) && identical(as.integer(slider_value), as.integer(iteration_value))) {
        return(NULL)
      }

      updateSliderInput(
        session = session,
        inputId = "iteration_slider",
        value = iteration_value
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
      format_iteration_status_text(model_results, current_iteration(), total_iteration_count())
    })

    output$playback_status_text <- renderText({
      model_results <- safe_model_results()
      format_iteration_navigation_status_text(model_results)
    })

    output$playback_helper_text <- renderText({
      model_results <- safe_model_results()
      active_iteration <- active_iteration_results()
      format_playback_helper_text(model_results, active_iteration)
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

      build_classification_plot(current_classification_data, active_model_view)
    }, res = 110)

    output$iteration_metric_plot <- renderPlot({
      model_results <- safe_model_results()

      if (!model_supports_logistic_playback(model_results)) {
        return(draw_empty_iteration_metric_plot())
      }

      metric_history <- model_results$iteration_metrics
      build_iteration_metric_plot(metric_history, current_iteration())
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

    list(
      plot_click_coordinates = reactive(input$plot_click),
      set_model_reactive = set_model_reactive
    )
  })
}
