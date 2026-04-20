# modules/raw_data_module.R
# Purpose:
#   Display the current classification dataset in a clean, styled table.
#   The class column is rendered as colored badges to match the reference image.
#
# Functions:
#   - raw_data_module_ui(): Build the Raw Data tab shell.
#   - raw_data_module_server(): Render the HTML data table.
#
# Inputs / Outputs:
#   Inputs:
#     - Reactive classification dataset
#   Outputs:
#     - Styled HTML table showing index, x, y, and class
#hola
raw_data_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "raw-data-tab-layout",
    tags$h3("Raw Data Points"),
    div(
      class = "raw-data-table-shell",
      uiOutput(ns("raw_data_table_ui"))
    )
  )
}


raw_data_module_server <- function(id, classification_data) {
  moduleServer(id, function(input, output, session) {
    output$raw_data_table_ui <- renderUI({
      current_classification_data <- classification_data()

      if (nrow(current_classification_data) == 0) {
        return(
          div(class = "empty-state-message", "No data is currently available.")
        )
      }

      table_rows <- lapply(seq_len(nrow(current_classification_data)), function(row_index) {
        current_row <- current_classification_data[row_index, , drop = FALSE]

        badge_class <- if (as.character(current_row$class) == "Class A") "class-badge class-a-badge" else "class-badge class-b-badge"

        tags$tr(
          tags$td(current_row$index),
          tags$td(sprintf("%.3f", current_row$x)),
          tags$td(sprintf("%.3f", current_row$y)),
          tags$td(tags$span(class = badge_class, as.character(current_row$class)))
        )
      })

      tags$table(
        class = "raw-data-table",
        tags$thead(
          tags$tr(
            tags$th("Index"),
            tags$th("X"),
            tags$th("Y"),
            tags$th("Class")
          )
        ),
        tags$tbody(table_rows)
      )
    })
  })
}
