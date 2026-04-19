# modules/dataset_controls_module.R
# Purpose:
#   Create the left sidebar sections for problem type selection and data setup.
#   This module is responsible for:
#   - showing classification as the active workflow
#   - loading preset datasets
#   - accepting CSV uploads
#   - toggling draw mode
#   - choosing which class to draw
#
# Functions:
#   - dataset_controls_module_ui(): Build sidebar sections 1 and 2.
#   - dataset_controls_module_server(): Expose the selected dataset,
#     uploaded data, draw mode state, and draw-related actions.
#
# Inputs / Outputs:
#   Inputs:
#     - Preset dataset dropdown
#     - Upload CSV control
#     - Draw Data, Undo Last, Clear All buttons
#     - Class A / Class B drawing toggle
#   Outputs:
#     - Reactive selected dataset name
#     - Reactive uploaded dataset
#     - Reactive draw mode state
#     - Reactive selected drawing class
#     - Reactive undo / clear button clicks

dataset_controls_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "sidebar-header-block",
      tags$h2("Build Your Model"),
      tags$p("Follow these steps to configure and run your classification workflow.")
    ),

    div(
      class = "sidebar-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "1"),
        tags$span("Select Problem Type")
      ),
      div(
        class = "problem-type-card is-active",
        tags$div(class = "problem-type-title-row",
          tags$span("Classification"),
          tags$span(class = "inline-status-badge", "Active")
        ),
        tags$p("Separate 2D points into Class A and Class B.")
      ),
      div(
        class = "problem-type-card is-disabled",
        tags$div(class = "problem-type-title-row",
          tags$span("Regression"),
          tags$span(class = "inline-muted-badge", "Disabled")
        ),
        tags$p("Hidden in this project so the app stays focused on classification.")
      ),
      div(
        class = "problem-type-card is-disabled",
        tags$div(class = "problem-type-title-row",
          tags$span("Clustering"),
          tags$span(class = "inline-muted-badge", "Disabled")
        ),
        tags$p("Not included in this version of the visualizer.")
      )
    ),

    div(
      class = "sidebar-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "2"),
        tags$span("Choose or Create Data")
      ),
      tags$p(
        class = "sidebar-helper-text",
        "Choose a preset dataset, upload a CSV, or turn on draw mode and click directly on the plot."
      ),
      tags$label(class = "sidebar-input-label", "Preset Datasets"),
      selectInput(
        inputId = ns("preset_dataset"),
        label = NULL,
        choices = c(
          "Gaussian clusters",
          "Linearly separable",
          "Overlapping classes",
          "Moons",
          "Circles"
        ),
        selected = "Gaussian clusters",
        width = "100%"
      ),
      tags$label(class = "sidebar-input-label", "Or Create Custom Data"),
      div(
        class = "button-row",
        actionButton(
          inputId = ns("draw_data_button"),
          label = "Draw Data",
          class = "ml-button ml-button-secondary half-width-button"
        ),
        div(
          class = "upload-button-shell",
          fileInput(
            inputId = ns("dataset_upload"),
            label = NULL,
            accept = c(".csv"),
            buttonLabel = "Upload CSV",
            placeholder = "No file selected"
          )
        )
      ),
      div(
        class = "draw-mode-status",
        textOutput(ns("drawing_mode_status"))
      ),
      tags$label(class = "sidebar-input-label", "Select Class For Drawing"),
      radioButtons(
        inputId = ns("drawing_class"),
        label = NULL,
        choices = c("Class A", "Class B"),
        selected = "Class A",
        inline = TRUE
      ),
      div(
        class = "button-row utility-button-row",
        actionButton(
          inputId = ns("undo_last_point_button"),
          label = "Undo Last",
          class = "ml-button ml-button-link"
        ),
        actionButton(
          inputId = ns("clear_points_button"),
          label = "Clear All",
          class = "ml-button ml-button-link"
        )
      )
    )
  )
}


dataset_controls_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    drawing_mode_active <- reactiveVal(FALSE)
    uploaded_dataset <- reactiveVal(NULL)

    observeEvent(input$draw_data_button, {
      drawing_mode_active(!drawing_mode_active())
    })

    observeEvent(input$dataset_upload, {
      req(input$dataset_upload$datapath)

      uploaded_table <- tryCatch(
        read.csv(input$dataset_upload$datapath, stringsAsFactors = FALSE),
        error = function(error_object) {
          showNotification(
            paste("Could not read the uploaded CSV:", error_object$message),
            type = "error"
          )
          NULL
        }
      )

      if (is.null(uploaded_table)) {
        return(NULL)
      }

      cleaned_dataset <- tryCatch(
        convert_uploaded_csv_to_dataset(uploaded_table),
        error = function(error_object) {
          showNotification(error_object$message, type = "error")
          NULL
        }
      )

      if (!is.null(cleaned_dataset)) {
        uploaded_dataset(cleaned_dataset)
        showNotification("CSV uploaded successfully. The visualizer is now using your custom dataset.", type = "message")
      }
    })

    output$drawing_mode_status <- renderText({
      if (drawing_mode_active()) {
        paste("Drawing mode is ON. Click inside the plot to add", input$drawing_class, "points.")
      } else {
        "Drawing mode is OFF. Press 'Draw Data' to start placing points on the plot."
      }
    })

    list(
      selected_dataset_name = reactive(input$preset_dataset),
      uploaded_dataset = reactive(uploaded_dataset()),
      drawing_mode_active = reactive(drawing_mode_active()),
      selected_drawing_class = reactive(input$drawing_class),
      undo_last_point = reactive(input$undo_last_point_button),
      clear_drawn_points = reactive(input$clear_points_button)
    )
  })
}
