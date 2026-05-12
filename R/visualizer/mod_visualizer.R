# R/visualizer/mod_visualizer.R
# Purpose:
#   Coordinate the full classification workflow page.
#   This module connects the sidebar controls, the interactive plot,
#   the raw data tab, and the model explanation tab.
#
# Functions:
#   - mod_visualizer_ui(): Build the two-column visualizer layout.
#   - mod_visualizer_server(): Manage dataset state, drawing state,
#     classifier training, and communication across child modules.
#
# Inputs / Outputs:
#   Inputs:
#     - Dataset selection / upload / drawing actions
#     - Algorithm selection and parameter changes
#     - Plot click events used for drawing custom data points
#   Outputs:
#     - Trained model results
#     - Updated classification plot
#     - Raw data table
#     - Model explanation content


visualizer_sidebar_results_ui <- function(ns) {
  div(
    class = "wizard-results-card",
    tags$div(class = "wizard-results-kicker", "Results ready"),
    tags$h3("Model ready"),
    tags$p(
      class = "wizard-results-note",
      "Inspect the visualization or edit the setup to run again."
    ),
    div(
      class = "wizard-results-actions",
      actionButton(
        inputId = ns("edit_setup_button"),
        label = "Edit setup",
        class = "ml-button ml-button-secondary ml-button-full",
        title = "Reopen the setup controls without clearing the current visualization."
      ),
      actionButton(
        inputId = ns("start_over_button"),
        label = "Start over",
        class = "ml-button ml-button-link wizard-start-over-button",
        title = "Clear the current model results and return to the beginning."
      )
    )
  )
}


mod_visualizer_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "visualizer-page",
    div(
      class = "visualizer-layout",
      div(
        class = "visualizer-sidebar",
        conditionalPanel(
          condition = paste0("output['", ns("wizard_state"), "'] != 'results'"),
          div(
            class = "wizard-setup-panel",
            mod_visualizer_dataset_controls_ui(ns("dataset_controls")),
            mod_visualizer_algorithm_controls_ui(ns("algorithm_controls"))
          )
        ),
        conditionalPanel(
          condition = paste0("output['", ns("wizard_state"), "'] == 'results'"),
          uiOutput(ns("wizard_results_ui"))
        )
      ),
      div(
        class = "visualizer-main-column",
        div(
          class = "app-card main-panel-card",
          div(
            class = "visualizer-primary-view",
            mod_visualizer_plot_panel_ui(ns("plot_panel"))
          ),
          div(
            class = "visualizer-secondary-tabs",
            tabsetPanel(
              id = ns("visualizer_tabs"),
              type = "tabs",
              selected = "Training insights",
              tabPanel(
                title = "Training insights",
                mod_visualizer_training_insights_ui(ns("plot_panel"))
              ),
              tabPanel(
                title = "Model Explanation",
                mod_visualizer_model_explanation_ui(ns("model_explanation_panel"))
              ),
              tabPanel(
                title = "Raw Data",
                mod_visualizer_raw_data_ui(ns("raw_data_panel"))
              )
            )
          )
        )
      )
    )
  )
}


mod_visualizer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    wizard_state <- reactiveVal("data")
    dataset_controls <- mod_visualizer_dataset_controls_server("dataset_controls")
    algorithm_controls <- mod_visualizer_algorithm_controls_server("algorithm_controls")

    output$wizard_state <- renderText({
      wizard_state()
    })
    outputOptions(output, "wizard_state", suspendWhenHidden = FALSE)

    # The visualizer keeps preset/uploaded data separate from user-drawn
    # points so drawing can be cleared without losing the selected base dataset.
    base_classification_data <- reactiveVal(generate_preset_dataset("Gaussian clusters"))
    drawn_classification_data <- reactiveVal(create_empty_classification_data())
    current_base_dataset_label <- reactiveVal("Gaussian clusters")

    # Choosing a preset replaces the base dataset and resets any drawn points.
    observeEvent(dataset_controls$selected_dataset_name(), {
      new_preset_data <- generate_preset_dataset(dataset_controls$selected_dataset_name())
      base_classification_data(new_preset_data)
      drawn_classification_data(create_empty_classification_data())
      current_base_dataset_label(dataset_controls$selected_dataset_name())

      if (identical(isolate(wizard_state()), "data")) {
        wizard_state("model")
      }
    }, ignoreInit = TRUE)

    # Uploads are already validated by mod_visualizer_dataset_controls_server().
    observeEvent(dataset_controls$uploaded_dataset(), {
      uploaded_classification_data <- dataset_controls$uploaded_dataset()

      if (!is.null(uploaded_classification_data)) {
        base_classification_data(uploaded_classification_data)
        drawn_classification_data(create_empty_classification_data())
        current_base_dataset_label("Uploaded CSV")

        if (identical(isolate(wizard_state()), "data")) {
          wizard_state("model")
        }
      }
    }, ignoreNULL = TRUE)

    observeEvent(algorithm_controls$selected_algorithm_key(), {
      current_state <- isolate(wizard_state())

      if (current_state %in% c("data", "model")) {
        wizard_state("parameters")
      }
    }, ignoreInit = TRUE)

    observeEvent(dataset_controls$clear_drawn_points(), {
      drawn_classification_data(create_empty_classification_data())
    })

    observeEvent(dataset_controls$undo_last_point(), {
      current_drawn_points <- drawn_classification_data()

      if (nrow(current_drawn_points) > 0) {
        updated_drawn_points <- current_drawn_points[-nrow(current_drawn_points), , drop = FALSE]
        updated_drawn_points$index <- seq_len(nrow(updated_drawn_points))
        drawn_classification_data(updated_drawn_points)
      }
    })

    # current_classification_data() is the single dataset passed to training,
    # plotting, and the raw data table.
    current_classification_data <- reactive({
      base_data <- base_classification_data()
      drawn_data <- drawn_classification_data()

      if (nrow(drawn_data) == 0) {
        combined_data <- base_data
      } else {
        combined_data <- rbind(
          data.frame(
            index = base_data$index,
            x = base_data$x,
            y = base_data$y,
            class = factor(base_data$class, levels = c("Class A", "Class B")),
            stringsAsFactors = FALSE
          ),
          data.frame(
            index = drawn_data$index,
            x = drawn_data$x,
            y = drawn_data$y,
            class = factor(drawn_data$class, levels = c("Class A", "Class B")),
            stringsAsFactors = FALSE
          )
        )
      }

      combined_data$index <- seq_len(nrow(combined_data))
      combined_data$class <- factor(combined_data$class, levels = c("Class A", "Class B"))
      combined_data
    })

    current_dataset_summary_label <- reactive({
      base_label <- current_base_dataset_label()

      if (is.null(base_label) || length(base_label) != 1 || is.na(base_label) || !nzchar(base_label)) {
        base_label <- "—"
      }

      if (nrow(drawn_classification_data()) > 0) {
        paste(base_label, "+ drawn points")
      } else {
        base_label
      }
    })

    model_run_count <- reactiveVal(0)

    # The plot panel owns the visible plot and iteration controls. The parent
    # passes reactive data/control state in and receives plot clicks back.
    plot_panel <- mod_visualizer_plot_panel_server(
      id = "plot_panel",
      classification_data = current_classification_data,
      drawing_mode_active = dataset_controls$drawing_mode_active,
      selected_class_label = dataset_controls$selected_drawing_class,
      run_model_clicked = reactive(model_run_count()),
      selected_dataset_label = current_dataset_summary_label,
      selected_algorithm_key = algorithm_controls$selected_algorithm_key,
      algorithm_parameters = algorithm_controls$algorithm_parameters
    )

    run_selected_model <- function(parameter_values) {
      wizard_state("run")

      trained_model_results <- tryCatch(
        train_classification_model(
          classification_data = current_classification_data(),
          algorithm_name = algorithm_controls$selected_algorithm_key(),
          parameter_values = parameter_values
        ),
        error = function(error_object) {
          showNotification(error_object$message, type = "error")
          NULL
        }
      )

      trained_model_bundle(trained_model_results)
      model_run_count(isolate(model_run_count()) + 1)

      if (!is.null(trained_model_results)) {
        wizard_state("results")
      }
    }

    # Plot clicks add custom points only while draw mode is enabled.
    observeEvent(plot_panel$plot_click_coordinates(), {
      if (!dataset_controls$drawing_mode_active()) {
        return(NULL)
      }

      clicked_point <- plot_panel$plot_click_coordinates()

      if (is.null(clicked_point$x) || is.null(clicked_point$y)) {
        return(NULL)
      }

      current_drawn_points <- drawn_classification_data()

      new_point <- data.frame(
        index = nrow(current_drawn_points) + 1,
        x = round(clicked_point$x, 3),
        y = round(clicked_point$y, 3),
        class = factor(dataset_controls$selected_drawing_class(), levels = c("Class A", "Class B")),
        stringsAsFactors = FALSE
      )

      updated_drawn_points <- rbind(current_drawn_points, new_point)
      updated_drawn_points$index <- seq_len(nrow(updated_drawn_points))
      drawn_classification_data(updated_drawn_points)
    })

    trained_model_bundle <- reactiveVal(NULL)

    output$wizard_results_ui <- renderUI({
      visualizer_sidebar_results_ui(ns = session$ns)
    })

    # The Run Classifier button is the training trigger. Parameter changes do
    # not retrain automatically; they are read only when this event fires.
    observeEvent(algorithm_controls$run_model_clicks(), {
      run_selected_model(algorithm_controls$algorithm_parameters())
    }, ignoreInit = TRUE)

    observeEvent(algorithm_controls$run_defaults_clicks(), {
      run_selected_model(algorithm_controls$default_algorithm_parameters())
    }, ignoreInit = TRUE)

    observeEvent(input$edit_setup_button, {
      wizard_state("parameters")
    }, ignoreInit = TRUE)

    observeEvent(input$start_over_button, {
      trained_model_bundle(NULL)
      model_run_count(0)
      drawn_classification_data(create_empty_classification_data())
      plot_panel$reset_display_state()
      wizard_state("data")
    }, ignoreInit = TRUE)

    plot_panel$set_model_reactive(trained_model_bundle)

    # Child modules receive the same reactive sources, so their displays stay
    # synchronized with the current data and latest trained model.
    mod_visualizer_raw_data_server(
      id = "raw_data_panel",
      classification_data = current_classification_data
    )

    mod_visualizer_model_explanation_server(
      id = "model_explanation_panel",
      selected_algorithm_key = algorithm_controls$selected_algorithm_key
    )
  })
}
