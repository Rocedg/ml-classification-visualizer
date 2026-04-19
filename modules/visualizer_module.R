# modules/visualizer_module.R
# Purpose:
#   Coordinate the full classification workflow page.
#   This module connects the sidebar controls, the interactive plot,
#   the raw data tab, and the model theory tab.
#
# Functions:
#   - visualizer_module_ui(): Build the two-column visualizer layout.
#   - visualizer_module_server(): Manage dataset state, drawing state,
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
#     - Model theory explanations

visualizer_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "visualizer-page",
    div(
      class = "visualizer-layout",
      div(
        class = "visualizer-sidebar",
        dataset_controls_module_ui(ns("dataset_controls")),
        algorithm_controls_module_ui(ns("algorithm_controls"))
      ),
      div(
        class = "visualizer-main-column",
        div(
          class = "app-card main-panel-card",
          tabsetPanel(
            id = ns("visualizer_tabs"),
            type = "tabs",
            selected = "Interactive Plot",
            tabPanel(
              title = "Interactive Plot",
              plot_panel_module_ui(ns("plot_panel"))
            ),
            tabPanel(
              title = "Model Theory",
              model_theory_panel_module_ui(ns("model_theory_panel"))
            ),
            tabPanel(
              title = "Raw Data",
              raw_data_module_ui(ns("raw_data_panel"))
            )
          )
        )
      )
    )
  )
}


visualizer_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    dataset_controls <- dataset_controls_module_server("dataset_controls")
    algorithm_controls <- algorithm_controls_module_server("algorithm_controls")

    base_classification_data <- reactiveVal(generate_preset_dataset("Gaussian clusters"))
    drawn_classification_data <- reactiveVal(create_empty_classification_data())

    observeEvent(dataset_controls$selected_dataset_name(), {
      new_preset_data <- generate_preset_dataset(dataset_controls$selected_dataset_name())
      base_classification_data(new_preset_data)
      drawn_classification_data(create_empty_classification_data())
    }, ignoreInit = FALSE)

    observeEvent(dataset_controls$uploaded_dataset(), {
      uploaded_classification_data <- dataset_controls$uploaded_dataset()

      if (!is.null(uploaded_classification_data)) {
        base_classification_data(uploaded_classification_data)
        drawn_classification_data(create_empty_classification_data())
      }
    }, ignoreNULL = TRUE)

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

    plot_panel <- plot_panel_module_server(
      id = "plot_panel",
      classification_data = current_classification_data,
      drawing_mode_active = dataset_controls$drawing_mode_active,
      selected_class_label = dataset_controls$selected_drawing_class,
      run_model_clicked = algorithm_controls$run_model_clicks
    )

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

    trained_model_bundle <- eventReactive(algorithm_controls$run_model_clicks(), {
      tryCatch(
        train_classification_model(
          classification_data = current_classification_data(),
          algorithm_name = algorithm_controls$selected_algorithm_key(),
          parameter_values = algorithm_controls$algorithm_parameters()
        ),
        error = function(error_object) {
          showNotification(error_object$message, type = "error")
          NULL
        }
      )
    }, ignoreInit = TRUE)

    plot_panel$set_model_reactive(trained_model_bundle)

    raw_data_module_server(
      id = "raw_data_panel",
      classification_data = current_classification_data
    )

    model_theory_panel_module_server(
      id = "model_theory_panel",
      selected_algorithm_key = algorithm_controls$selected_algorithm_key,
      trained_model_bundle = trained_model_bundle
    )
  })
}
