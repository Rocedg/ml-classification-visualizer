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
  help_icon <- function(help_text) {
    tags$span(
      class = "help-tooltip",
      `aria-label` = help_text,
      "?"
    )
  }

  div(
    class = "plot-tab-layout",
    div(
      class = "main-plot-header",
      div(
        class = "main-plot-title-row",
        tags$h3(class = "main-plot-title", "Main visualization")
      ),
      div(
        class = "plot-top-status-row",
        div(
          class = "status-chip status-chip-primary",
          textOutput(ns("plot_status_text"), inline = TRUE)
        ),
        div(class = "status-chip", textOutput(ns("drawing_status_text"), inline = TRUE))
      )
    ),
    div(
      class = "plot-canvas-shell main-feature-plot-card",
      title = "Background color shows how the selected classifier divides the feature space.",
      div(
        class = "main-feature-plot-shell",
        plotOutput(
          outputId = ns("classification_plot"),
          height = "100%",
          click = ns("plot_click")
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("probability_guide_visible"), "'] == 'true'"),
        div(
          class = "plot-guide-stack",
          div(
            class = "probability-guide-panel",
            title = "Background colors explain the selected model output across the feature space.",
            tags$div(
              class = "probability-guide-title",
              uiOutput(ns("region_guide_title_ui"), inline = TRUE)
            ),
            tags$div(
              class = "probability-guide-body",
              uiOutput(ns("region_guide_gradient_ui")),
              tags$div(
                class = "probability-guide-labels",
                tags$span(textOutput(ns("region_guide_top_label"), inline = TRUE)),
                tags$span(textOutput(ns("region_guide_middle_label"), inline = TRUE)),
                tags$span(textOutput(ns("region_guide_bottom_label"), inline = TRUE))
              )
            )
          ),
          div(
            class = "point-guide-panel",
            div(
              class = "point-guide-title",
              tags$span("Points"),
              help_icon("Training points are used to fit the model. Test points are held out for evaluation.")
            ),
            div(
              class = "point-guide-list",
              div(class = "point-guide-row", tags$span(class = "point-guide-dot point-guide-dot-class-a"), tags$span("Class A")),
              div(class = "point-guide-row", tags$span(class = "point-guide-dot point-guide-dot-class-b"), tags$span("Class B")),
              div(class = "point-guide-row", tags$span(class = "point-guide-dot point-guide-dot-train"), tags$span("Train")),
              div(class = "point-guide-row", tags$span(class = "point-guide-dot point-guide-dot-test"), tags$span("Test")),
              conditionalPanel(
                condition = paste0("output['", ns("svm_guide_visible"), "'] == 'true'"),
                div(class = "point-guide-row", tags$span(class = "point-guide-dot point-guide-dot-support-vector"), tags$span("Support vector"))
              )
            )
          )
        )
      )
    ),
    visualizer_interaction_panel_ui(ns),
    visualizer_summary_cards_ui(ns, help_icon)
  )
}


mod_visualizer_plot_panel_server <- function(id,
                                             classification_data,
                                             drawing_mode_active,
                                             selected_class_label,
                                             run_model_clicked,
                                             selected_dataset_label = reactive("—"),
                                             selected_algorithm_key = reactive("logistic_regression"),
                                             algorithm_parameters = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    # The parent module supplies the trained model through this setter after
    # the child module is created.
    internal_model_reactive <- reactiveVal(NULL)
    knn_selected_query_point <- reactiveVal(NULL)

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

    selected_algorithm_text <- reactive({
      algorithm_key <- selected_algorithm_key()

      if (is.null(algorithm_key) || length(algorithm_key) != 1 || is.na(algorithm_key)) {
        return("logistic_regression")
      }

      as.character(algorithm_key)
    })

    output$show_logistic_playback_controls <- renderText({
      if (identical(selected_algorithm_text(), "logistic_regression")) "true" else "false"
    })
    outputOptions(output, "show_logistic_playback_controls", suspendWhenHidden = FALSE)

    output$show_knn_iteration_note <- renderText({
      if (identical(selected_algorithm_text(), "knn")) "true" else "false"
    })
    outputOptions(output, "show_knn_iteration_note", suspendWhenHidden = FALSE)

    output$show_svm_summary_panel <- renderText({
      if (identical(selected_algorithm_text(), "svm")) "true" else "false"
    })
    outputOptions(output, "show_svm_summary_panel", suspendWhenHidden = FALSE)

    output$training_insights_algorithm <- renderText({
      selected_algorithm_text()
    })
    outputOptions(output, "training_insights_algorithm", suspendWhenHidden = FALSE)

    output$training_insights_subtitle <- renderText({
      if (identical(selected_algorithm_text(), "knn")) {
        "Review how nearest-neighbor voting makes predictions."
      } else if (identical(selected_algorithm_text(), "svm")) {
        "Review the margin, support vectors, and C."
      } else {
        "Track loss and parameter movement."
      }
    })

    # Before the Run Classifier button has been clicked, the plot displays only
    # data points. After a run, this reactive unwraps the latest model bundle.
    safe_model_results <- reactive({
      model_reactive_expression <- internal_model_reactive()

      if (is.null(model_reactive_expression) || run_model_clicked() == 0) {
        return(NULL)
      }

      model_results <- model_reactive_expression()

      if (is.null(model_results) || !identical(model_results$algorithm_key, selected_algorithm_text())) {
        return(NULL)
      }

      model_results
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

    active_knn_inspection <- reactive({
      if (!identical(selected_algorithm_text(), "knn")) {
        return(NULL)
      }

      model_results <- safe_model_results()
      query_point <- knn_selected_query_point()

      if (is.null(model_results) || !identical(model_results$algorithm_key, "knn") || is.null(query_point)) {
        return(NULL)
      }

      parameter_values <- algorithm_parameters()
      requested_k <- parameter_values$knn_k
      distance_metric <- parameter_values$knn_distance_metric
      voting_method <- parameter_values$knn_voting_method

      if (is.null(requested_k)) {
        requested_k <- model_results$model_object$effective_k
      }
      if (is.null(requested_k)) {
        requested_k <- 5
      }
      if (is.null(distance_metric)) {
        distance_metric <- model_results$model_object$distance_metric
      }
      if (is.null(voting_method)) {
        voting_method <- model_results$model_object$voting_method
      }

      inspect_knn_point(
        training_data = model_results$train_data,
        query_point = query_point,
        k = requested_k,
        distance_metric = distance_metric,
        voting_method = voting_method
      )
    })

    training_data_for_diagnostics <- reactive({
      model_results <- safe_model_results()

      if (model_supports_logistic_playback(model_results) && !is.null(model_results$train_data)) {
        return(model_results$train_data)
      }

      classification_data()
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

    observeEvent(selected_algorithm_text(), {
      if (!identical(selected_algorithm_text(), "logistic_regression")) {
        advance_playback_generation()
        is_playing(FALSE)
      }

      if (!identical(selected_algorithm_text(), "knn")) {
        knn_selected_query_point(NULL)
      }
    }, ignoreInit = TRUE)

    observeEvent(classification_data(), {
      knn_selected_query_point(NULL)
    }, ignoreInit = TRUE)

    observeEvent(run_model_clicked(), {
      knn_selected_query_point(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$plot_click, {
      if (!identical(selected_algorithm_text(), "knn") || drawing_mode_active()) {
        return(NULL)
      }

      model_results <- safe_model_results()
      if (is.null(model_results) || !identical(model_results$algorithm_key, "knn")) {
        return(NULL)
      }

      clicked_point <- input$plot_click
      if (is.null(clicked_point$x) || is.null(clicked_point$y)) {
        return(NULL)
      }

      clicked_x <- suppressWarnings(as.numeric(clicked_point$x))
      clicked_y <- suppressWarnings(as.numeric(clicked_point$y))
      if (is.na(clicked_x) || is.na(clicked_y)) {
        return(NULL)
      }

      active_model_view <- active_iteration_results()
      prediction_grid <- active_model_view$prediction_grid

      if (!is.null(prediction_grid) && all(c("x", "y") %in% names(prediction_grid))) {
        x_range <- range(prediction_grid$x, finite = TRUE)
        y_range <- range(prediction_grid$y, finite = TRUE)

        if (length(x_range) == 2 && length(y_range) == 2 &&
            (clicked_x < x_range[1] || clicked_x > x_range[2] ||
             clicked_y < y_range[1] || clicked_y > y_range[2])) {
          return(NULL)
        }
      }

      knn_selected_query_point(data.frame(x = clicked_x, y = clicked_y))
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
    # The UI sends this command explicitly from onclick so playback does not
    # depend on the actionButton counter while the label is being updated.
    observeEvent(input$play_pause_command, {
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
      if (identical(selected_algorithm_text(), "knn")) {
        return("k-NN inspection")
      }

      if (identical(selected_algorithm_text(), "svm")) {
        return("SVM margin summary")
      }

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

    output$knn_inspection_ui <- renderUI({
      visualizer_knn_inspection_panel_ui(
        selected_algorithm = selected_algorithm_text(),
        model_results = safe_model_results(),
        inspection = active_knn_inspection()
      )
    })
    output$svm_summary_ui <- renderUI({
      visualizer_svm_margin_panel_ui(
        selected_algorithm = selected_algorithm_text(),
        model_results = safe_model_results()
      )
    })
    output$current_run_summary <- renderUI({
      parameter_values <- algorithm_parameters()
      model_results <- safe_model_results()

      if (is.null(parameter_values)) {
        parameter_values <- list()
      }

      iteration_text <- if (total_iteration_count() > 0) {
        paste(current_iteration(), "/", total_iteration_count() - 1)
      } else {
        "Not started"
      }

      visualizer_current_run_summary_ui(
        parameter_values = parameter_values,
        model_results = model_results,
        selected_dataset_label = selected_dataset_label(),
        algorithm_key = selected_algorithm_key(),
        iteration_text = iteration_text
      )
    })
    output$metrics_summary_ui <- renderUI({
      active_model_view <- active_iteration_results()
      train_metrics <- if (is.null(active_model_view)) NULL else active_model_view$train_metrics
      test_metrics <- if (is.null(active_model_view)) NULL else active_model_view$test_metrics

      visualizer_metrics_summary_ui(train_metrics, test_metrics)
    })
    output$svm_training_summary_ui <- renderUI({
      model_results <- safe_model_results()

      if (is.null(model_results) || !identical(model_results$algorithm_key, "svm")) {
        return(tags$p("Run SVM to see support vector counts for the current split."))
      }

      margin_summary <- model_results$margin_summary

      if (is.null(margin_summary)) {
        margin_summary <- list()
      }

      tags$ul(
        tags$li(paste("Kernel: Linear")),
        tags$li(paste("C:", format_current_run_number(margin_summary$cost))),
        tags$li(paste("Support vectors:", format_current_run_integer(margin_summary$support_vector_count))),
        tags$li("Decision boundary: score = 0"),
        tags$li("Margin contours: score = -1 and +1 when available")
      )
    })
    output$probability_guide_visible <- renderText({
      if (is.null(active_iteration_results())) "false" else "true"
    })
    outputOptions(output, "probability_guide_visible", suspendWhenHidden = FALSE)

    active_plot_uses_svm <- reactive({
      active_model_view <- active_iteration_results()
      !is.null(active_model_view) && identical(active_model_view$algorithm_key, "svm")
    })

    guide_help_icon <- function(help_text) {
      tags$span(
        class = "help-tooltip",
        `aria-label` = help_text,
        "?"
      )
    }

    output$svm_guide_visible <- renderText({
      if (active_plot_uses_svm()) "true" else "false"
    })
    outputOptions(output, "svm_guide_visible", suspendWhenHidden = FALSE)

    output$region_guide_title_ui <- renderUI({
      if (active_plot_uses_svm()) {
        return(tagList(
          tags$span("Decision regions"),
          guide_help_icon("Blue and orange show the SVM predicted class regions. The solid contour is the decision boundary, and dashed contours show the margins when available.")
        ))
      }

      tagList(
        tags$span("Probability"),
        guide_help_icon("Blue means Class A is more likely, orange means Class B is more likely, and the middle region is uncertain.")
      )
    })

    output$region_guide_gradient_ui <- renderUI({
      guide_class <- if (active_plot_uses_svm()) {
        "probability-guide-gradient decision-region-guide-gradient"
      } else {
        "probability-guide-gradient"
      }

      tags$div(class = guide_class)
    })

    output$region_guide_top_label <- renderText({
      if (active_plot_uses_svm()) "Class A region" else "Class A likely"
    })

    output$region_guide_middle_label <- renderText({
      if (active_plot_uses_svm()) "Boundary / margins" else "Uncertain"
    })

    output$region_guide_bottom_label <- renderText({
      if (active_plot_uses_svm()) "Class B region" else "Class B likely"
    })

    output$classification_plot <- renderPlot({
      current_classification_data <- classification_data()
      active_model_view <- active_iteration_results()
      knn_inspection <- active_knn_inspection()

      # active_model_view is either NULL or one saved training iteration. The
      # plot helper uses it to decide whether to draw the probability heatmap.
      build_classification_plot(current_classification_data, active_model_view, knn_inspection)
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
            classification_data = training_data_for_diagnostics(),
            iteration_history = NULL,
            current_iteration = current_iteration() + 1
          )
        )
      }

      if (model_uses_fit_intercept()) {
        return(
          build_bias_fixed_loss_landscape_plot(
            classification_data = training_data_for_diagnostics(),
            iteration_history = NULL,
            current_iteration = current_iteration() + 1
          )
        )
      }

      build_bias_fixed_loss_landscape_plot(
        classification_data = training_data_for_diagnostics(),
        iteration_history = iteration_history,
        current_iteration = current_iteration() + 1
      )
    }, res = 110)

    list(
      plot_click_coordinates = reactive(input$plot_click),
      set_model_reactive = set_model_reactive
    )
  })
}
