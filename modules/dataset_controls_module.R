# modules/dataset_controls_module.R
# Purpose:
#   Create the left sidebar section for data setup.
#   This module is responsible for:
#   - loading preset datasets
#   - accepting CSV uploads
#   - downloading a CSV template
#   - toggling draw mode
#   - choosing which class to draw
#
# Functions:
#   - dataset_controls_module_ui(): Build the data section.
#   - dataset_controls_module_server(): Expose the selected dataset,
#     uploaded data, draw mode state, and draw-related actions.
#
# Inputs / Outputs:
#   Inputs:
#     - Preset dataset dropdown
#     - Upload CSV control
#     - Download CSV Template button
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
      tags$p("Configure and run a classification workflow.")
    ),
    
    div(
      class = "sidebar-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "1"),
        tags$span("Choose or Create Data")
      ),
      
      tags$p(
        class = "sidebar-helper-text",
        "Choose a preset dataset, upload a CSV, download a template, or draw data directly on the plot."
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
          "Circles",
          "Titanic passengers",
          "Diabetes health data"
        ),
        selected = "Gaussian clusters",
        width = "100%"
      ),
      
      tags$label(class = "sidebar-input-label", "Or Create Custom Data"),
      
      div(
        class = "custom-data-buttons-vertical",
        
        actionButton(
          inputId = ns("draw_data_button"),
          label = "Draw Data",
          class = "ml-button ml-button-secondary custom-data-button"
        ),
        
        div(
          class = "custom-upload-button",
          fileInput(
            inputId = ns("dataset_upload"),
            label = NULL,
            accept = c(".csv"),
            buttonLabel = "Upload CSV",
            placeholder = "No file selected"
          )
        ),
        
        downloadButton(
          outputId = ns("download_example_csv"),
          label = "Template",
          class = "ml-button ml-button-secondary custom-data-button"
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
    # These reactive values are returned to the parent visualizer module.
    # The parent decides how they affect the shared dataset.
    drawing_mode_active <- reactiveVal(FALSE)
    uploaded_dataset <- reactiveVal(NULL)
    
    # Draw mode is a toggle: each click switches between collecting plot clicks
    # and ignoring them.
    observeEvent(input$draw_data_button, {
      drawing_mode_active(!drawing_mode_active())
    })
    
    output$download_example_csv <- downloadHandler(
      filename = function() {
        "ml_visualizer_csv_template.csv"
      },
      content = function(file) {
        example_template <- data.frame(
          x = c(-2.5, 2.5, rep(NA, 10)),
          y = c(2.0, -2.0, rep(NA, 10)),
          class = c("A", "B", rep("", 10)),
          stringsAsFactors = FALSE
        )
        
        write.csv(example_template, file, row.names = FALSE)
      }
    )
    
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
        # Store the cleaned dataset so the parent module can replace the
        # current base data with it.
        uploaded_dataset(cleaned_dataset)
        showNotification(
          "CSV uploaded successfully. The visualizer is now using your custom dataset.",
          type = "message"
        )
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
      # Expose plain reactives instead of raw input values so the parent module
      # does not need to know about this module's internal input IDs.
      selected_dataset_name = reactive(input$preset_dataset),
      uploaded_dataset = reactive(uploaded_dataset()),
      drawing_mode_active = reactive(drawing_mode_active()),
      selected_drawing_class = reactive(input$drawing_class),
      undo_last_point = reactive(input$undo_last_point_button),
      clear_drawn_points = reactive(input$clear_points_button)
    )
  })
}
