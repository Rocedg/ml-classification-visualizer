# R/visualizer/mod_visualizer_plot_panel.R
# Purpose:
#   Render the main interactive plot tab.
#   This module draws:
#   - the input data points
#   - the Logistic Regression probability heatmap
#   - the Logistic Regression parameter trajectory
#   - metric summary cards
#   It also exposes plot click coordinates back to the parent module
#   so users can draw their own points.
#
# Functions:
#   - mod_visualizer_plot_panel_ui(): Build the plot tab layout.
#   - mod_visualizer_plot_panel_server(): Render the plot, metric cards,
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

mod_visualizer_plot_panel_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "plot-tab-layout",
    div(
      class = "plot-top-status-row",
      div(class = "status-chip status-chip-primary", textOutput(ns("plot_status_text"), inline = TRUE)),
      div(class = "status-chip", textOutput(ns("drawing_status_text"), inline = TRUE))
    ),
    div(
      class = "plot-canvas-shell main-feature-plot-shell",
      plotOutput(
        outputId = ns("classification_plot"),
        height = "100%",
        click = ns("plot_click")
      )
    ),
    div(
      class = "iteration-control-card",
      div(
        class = "iteration-control-header",
        div(class = "status-chip status-chip-primary", textOutput(ns("iteration_status_text"), inline = TRUE))
      ),
      div(
        class = "iteration-control-body",
        div(
          class = "button-row iteration-button-row",
          actionButton(
            inputId = ns("step_backward_button"),
            label = "←",
            class = "ml-button ml-button-secondary iteration-icon-button"
          ),
          actionButton(
            inputId = ns("play_pause_button"),
            label = "▶",
            class = "ml-button ml-button-secondary iteration-icon-button"
          ),
          actionButton(
            inputId = ns("step_forward_button"),
            label = "→",
            class = "ml-button ml-button-secondary iteration-icon-button"
          )
        ),
        sliderInput(
          inputId = ns("iteration_slider"),
          label = NULL,
          min = 0,
          max = 0,
          value = 0,
          step = 1,
          width = "100%"
        )
      )
    ),
    div(
      class = "diagnostic-grid",
      div(
        class = "plot-canvas-shell diagnostic-plot-shell",
        plotOutput(
          outputId = ns("iteration_metric_plot"),
          height = "280px"
        )
      ),
      div(
        class = "app-card theory-summary-card diagnostic-plot-shell parameter-diagnostic-card",
        div(
          class = "plot-top-status-row",
          div(class = "status-chip status-chip-primary", textOutput(ns("parameter_diagnostic_title"), inline = TRUE))
        ),
        conditionalPanel(
          condition = paste0("output['", ns("show_3d_parameter_diagnostic"), "'] == 'true'"),
          plotly::plotlyOutput(
            outputId = ns("parameter_trajectory_3d_plot"),
            height = "280px"
          )
        ),
        conditionalPanel(
          condition = paste0("output['", ns("show_3d_parameter_diagnostic"), "'] == 'false'"),
          plotOutput(
            outputId = ns("bias_fixed_loss_landscape_plot"),
            height = "280px"
          )
        )
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


mod_visualizer_plot_panel_server <- function(id,
                                             classification_data,
                                             drawing_mode_active,
                                             selected_class_label,
                                             run_model_clicked) {
  moduleServer(id, function(input, output, session) {
    # The parent module supplies the trained model through this setter after
    # the child module is created.
    internal_model_reactive <- reactiveVal(NULL)

    # current_iteration is local display state using 0-based indexing (0 to max_iterations).
    # It controls which saved logistic training step is rendered without retraining the model.
    current_iteration <- reactiveVal(0)

    # is_playing tracks whether the Play button is active
    is_playing <- reactiveVal(FALSE)
    playback_generation <- reactiveVal(0)

    # Programmatic slider updates can echo back through input$iteration_slider.
    # Keep a small pending queue so those echoes cannot overwrite playback state.
    pending_programmatic_slider_values <- reactiveVal(integer(0))

    mark_programmatic_slider_update <- function(iteration_value) {
      pending_programmatic_slider_values(c(
        isolate(pending_programmatic_slider_values()),
        as.integer(iteration_value)
      ))
    }

    consume_programmatic_slider_update <- function(iteration_value) {
      pending_values <- isolate(pending_programmatic_slider_values())
      matched_position <- match(as.integer(iteration_value), pending_values)

      if (is.na(matched_position)) {
        return(FALSE)
      }

      pending_programmatic_slider_values(pending_values[-matched_position])
      TRUE
    }

    advance_playback_generation <- function() {
      playback_generation(isolate(playback_generation()) + 1)
    }

    set_model_reactive <- function(model_reactive_expression) {
      internal_model_reactive(model_reactive_expression)
    }

    # Before the Run Classifier button has been clicked, the plot displays only
    # data points. After a run, this reactive unwraps the latest model bundle.
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

    # A new model run resets playback to the first saved state (iteration 0) and
    # resizes the slider to match the number of stored iterations.
    # With 60 iterations requested, we have 61 saved states (0 through 60),
    # so slider max is 60.
    observeEvent(safe_model_results(), {
      model_results <- safe_model_results()

      current_iteration(0)
      advance_playback_generation()
      is_playing(FALSE)

      if (model_supports_logistic_playback(model_results)) {
        mark_programmatic_slider_update(0)
        updateSliderInput(
          session = session,
          inputId = "iteration_slider",
          min = 0,
          max = length(model_results$iterations) - 1,
          value = 0
        )
      } else {
        mark_programmatic_slider_update(0)
        updateSliderInput(
          session = session,
          inputId = "iteration_slider",
          min = 0,
          max = 0,
          value = 0
        )
      }
    }, ignoreInit = TRUE)

    # Moving the slider selects a saved iteration using 0-based indexing.
    # Only update current_iteration if user actually moved the slider (not from updateSliderInput).
    # Detect user change: if input value differs from current_iteration, user moved it.
    observeEvent(input$iteration_slider, {
      total_iterations <- total_iteration_count()
      if (total_iterations == 0 || is.null(input$iteration_slider)) {
        return(NULL)
      }

      bounded_iteration <- bound_iteration_index(input$iteration_slider, total_iterations)
      current_val <- isolate(current_iteration())

      if (consume_programmatic_slider_update(bounded_iteration)) {
        return(NULL)
      }

      # Only respond if slider value differs from our current iteration
      # (Indicates user moved it, not our updateSliderInput call)
      if (!identical(as.integer(input$iteration_slider), as.integer(current_val))) {
        current_iteration(bounded_iteration)
        is_playing(FALSE)
      }
    }, ignoreInit = TRUE)

    # Step Forward: advance by 1, cannot go above max_iterations
    observeEvent(input$step_forward_button, {
      total_iterations <- total_iteration_count()
      if (total_iterations == 0) {
        return(NULL)
      }

      max_iteration <- total_iterations - 1
      current_iteration(min(current_iteration() + 1, max_iteration))
      is_playing(FALSE)
    })

    # Step Back: go back by 1, cannot go below 0
    observeEvent(input$step_backward_button, {
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      current_iteration(max(current_iteration() - 1, 0))
      is_playing(FALSE)
    })

    # Play/Pause button: start from the current iteration, or pause immediately.
    observeEvent(input$play_pause_button, {
      total_iterations <- total_iteration_count()
      if (total_iterations == 0) {
        return(NULL)
      }

      advance_playback_generation()

      if (isTRUE(isolate(is_playing()))) {
        is_playing(FALSE)
        return(NULL)
      }

      if (isolate(current_iteration()) >= total_iterations - 1) {
        return(NULL)
      }

      is_playing(TRUE)
    }, ignoreInit = TRUE, priority = 100)

    # Auto-advance when playing: move forward every 1 second, stop at max
    # Uses isolate() on current_iteration read to break reactive dependency loop
    observe({
      req(is_playing())
      current_playback_generation <- playback_generation()
      invalidateLater(1000, session)

      if (!isTRUE(isolate(is_playing())) ||
          !identical(current_playback_generation, isolate(playback_generation()))) {
        return()
      }

      total_iterations <- isolate(total_iteration_count())
      if (total_iterations == 0) {
        advance_playback_generation()
        is_playing(FALSE)
        return()
      }

      max_iteration <- total_iterations - 1
      new_iteration <- isolate(current_iteration()) + 1

      if (new_iteration > max_iteration) {
        advance_playback_generation()
        is_playing(FALSE)
      } else {
        bounded_iteration <- min(new_iteration, max_iteration)
        current_iteration(bounded_iteration)

        if (bounded_iteration >= max_iteration) {
          advance_playback_generation()
          is_playing(FALSE)
        }
      }
    })

    # Keep the slider handle synchronized when current_iteration changes (single source of truth).
    # Always update to ensure slider stays in sync, even if values seem to match.
    observe({
      if (total_iteration_count() == 0) {
        return(NULL)
      }

      iteration_value <- current_iteration()

      mark_programmatic_slider_update(iteration_value)
      updateSliderInput(
        session = session,
        inputId = "iteration_slider",
        value = iteration_value
      )
    })

    # Update Play/Pause button label with Unicode icons
    observe({
      label <- if (is_playing()) "⏸" else "▶"
      shiny::updateActionButton(
        session = session,
        inputId = "play_pause_button",
        label = label
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

      # active_model_view is either NULL or one saved training iteration. The
      # plot helper uses it to decide whether to draw the probability heatmap.
      build_classification_plot(current_classification_data, active_model_view)
    }, res = 110)

    output$iteration_metric_plot <- renderPlot({
      model_results <- safe_model_results()

      if (!model_supports_logistic_playback(model_results)) {
        return(draw_empty_iteration_metric_plot())
      }

      metric_history <- model_results$iteration_metrics
      build_iteration_metric_plot(metric_history, current_iteration() + 1)
    }, res = 110)

    model_uses_fit_intercept <- reactive({
      model_results <- safe_model_results()

      if (!model_supports_logistic_playback(model_results)) {
        return(TRUE)
      }

      fit_intercept <- model_results$model_object$fit_intercept
      is.null(fit_intercept) || isTRUE(fit_intercept)
    })

    output$show_3d_parameter_diagnostic <- renderText({
      if (model_uses_fit_intercept()) "true" else "false"
    })
    outputOptions(output, "show_3d_parameter_diagnostic", suspendWhenHidden = FALSE)

    # With a learned intercept, the parameter path is 3D: weight_x, weight_y,
    # and bias. When fit_intercept is off, bias is fixed at 0, so a 2D loss
    # landscape over the two weights is shown instead.
    output$parameter_diagnostic_title <- renderText({
      if (model_uses_fit_intercept()) {
        "3D parameter trajectory"
      } else {
        "2D loss landscape"
      }
    })

    output$parameter_trajectory_3d_plot <- plotly::renderPlotly({
      model_results <- safe_model_results()
      iteration_history <- logistic_iteration_history()

      if (!model_supports_logistic_playback(model_results)) {
        return(build_empty_parameter_trajectory_3d_plot())
      }

      if (!model_uses_fit_intercept()) {
        return(build_empty_parameter_trajectory_3d_plot())
      }

      build_parameter_trajectory_3d_plot(
        iteration_history = iteration_history,
        current_iteration = current_iteration() + 1
      )
    })

    output$bias_fixed_loss_landscape_plot <- renderPlot({
      model_results <- safe_model_results()
      iteration_history <- logistic_iteration_history()

      if (!model_supports_logistic_playback(model_results)) {
        return(
          build_bias_fixed_loss_landscape_plot(
            classification_data = classification_data(),
            iteration_history = NULL,
            current_iteration = current_iteration() + 1
          )
        )
      }

      if (model_uses_fit_intercept()) {
        return(
          build_bias_fixed_loss_landscape_plot(
            classification_data = classification_data(),
            iteration_history = NULL,
            current_iteration = current_iteration() + 1
          )
        )
      }

      build_bias_fixed_loss_landscape_plot(
        classification_data = classification_data(),
        iteration_history = iteration_history,
        current_iteration = current_iteration() + 1,
        model_object = model_results$model_object
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

    list(
      plot_click_coordinates = reactive(input$plot_click),
      set_model_reactive = set_model_reactive
    )
  })
}
