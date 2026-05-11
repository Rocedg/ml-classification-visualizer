# R/visualizer/mod_visualizer_interaction_panel.R
# Purpose:
#   Build the below-plot interaction panel for model-specific controls.

visualizer_interaction_panel_ui <- function(ns) {
  div(
    class = "iteration-control-card",
    div(
      class = "iteration-control-header",
      div(
        class = "status-chip status-chip-primary",
        title = "Use the playback controls or slider to inspect saved training iterations.",
        textOutput(ns("iteration_status_text"), inline = TRUE)
      )
    ),
    div(
      class = "iteration-control-body",
      conditionalPanel(
        condition = paste0("output['", ns("show_logistic_playback_controls"), "'] == 'true'"),
        div(
          class = "iteration-control-row",
          div(
            class = "button-row iteration-button-row",
            actionButton(
              inputId = ns("step_backward_button"),
              label = HTML("&larr;"),
              class = "ml-button ml-button-secondary iteration-icon-button"
            ),
            actionButton(
              inputId = ns("play_pause_button"),
              label = "â–¶",
              class = "ml-button ml-button-secondary iteration-icon-button",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now() + Math.random(), {priority: 'event'});",
                ns("play_pause_command")
              )
            ),
            actionButton(
              inputId = ns("step_forward_button"),
              label = HTML("&rarr;"),
              class = "ml-button ml-button-secondary iteration-icon-button"
            )
          ),
          div(
            class = "iteration-slider-group",
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
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("show_knn_iteration_note"), "'] == 'true'"),
        uiOutput(ns("knn_inspection_ui"))
      )
    )
  )
}


format_inspection_number <- function(value, digits = 2) {
  if (is.null(value) || length(value) != 1 || is.na(value) || !is.finite(value)) {
    return("-")
  }

  formatC(as.numeric(value), format = "f", digits = digits)
}


visualizer_knn_inspection_panel_ui <- function(selected_algorithm, model_results, inspection) {
  if (!identical(selected_algorithm, "knn")) {
    return(NULL)
  }

  if (is.null(model_results) || !identical(model_results$algorithm_key, "knn")) {
    return(div(
      class = "knn-inspection-panel",
      tags$h4("k-NN inspection"),
      tags$p("Run k-NN, then click the plot to inspect the k nearest training points.")
    ))
  }

  if (is.null(inspection)) {
    return(div(
      class = "knn-inspection-panel",
      tags$h4("k-NN inspection"),
      tags$p("Click the plot to inspect the k nearest training points.")
    ))
  }

  neighbors <- inspection$neighbors

  if (is.null(neighbors) || nrow(neighbors) == 0) {
    return(div(
      class = "knn-inspection-panel",
      tags$h4("k-NN inspection"),
      tags$p("No valid training points are available for this inspection.")
    ))
  }

  vote_counts <- inspection$vote_counts
  class_a_votes <- as.integer(vote_counts[["Class A"]])
  class_b_votes <- as.integer(vote_counts[["Class B"]])
  predicted_class <- if (is.na(inspection$predicted_class)) "-" else inspection$predicted_class
  voting_method <- normalize_knn_voting_method(inspection$voting_method)
  show_weight_column <- identical(voting_method, "distance_weighted") && "weight" %in% names(neighbors)

  if (show_weight_column) {
    weighted_votes <- inspection$weighted_votes
    class_a_weighted_vote <- if (is.null(weighted_votes[["Class A"]])) 0 else weighted_votes[["Class A"]]
    class_b_weighted_vote <- if (is.null(weighted_votes[["Class B"]])) 0 else weighted_votes[["Class B"]]
    vote_label <- "Weighted vote"
    vote_value <- paste0(
      "Class A = ", format_inspection_number(class_a_weighted_vote),
      ", Class B = ", format_inspection_number(class_b_weighted_vote)
    )
  } else {
    vote_label <- "Vote"
    vote_value <- paste0("Class A = ", class_a_votes, ", Class B = ", class_b_votes)
  }

  neighbor_rows <- lapply(seq_len(nrow(neighbors)), function(row_index) {
    neighbor <- neighbors[row_index, , drop = FALSE]
    badge_class <- if (as.character(neighbor$class) == "Class A") "class-badge class-a-badge" else "class-badge class-b-badge"

    row_cells <- list(
      tags$td(neighbor$rank),
      tags$td(tags$span(class = badge_class, as.character(neighbor$class))),
      tags$td(format_inspection_number(neighbor$distance))
    )

    if (show_weight_column) {
      row_cells <- c(row_cells, list(tags$td(format_inspection_number(neighbor$weight))))
    }

    do.call(tags$tr, row_cells)
  })

  table_header_cells <- list(
    tags$th("Rank"),
    tags$th("Class"),
    tags$th("Distance")
  )

  if (show_weight_column) {
    table_header_cells <- c(table_header_cells, list(tags$th("Weight")))
  }

  div(
    class = "knn-inspection-panel",
    tags$h4("k-NN inspection"),
    div(
      class = "knn-inspection-grid",
      tags$span(class = "knn-inspection-label", "Selected point"),
      tags$span(
        class = "knn-inspection-value",
        paste0("x = ", format_inspection_number(inspection$query_point$x), ", y = ", format_inspection_number(inspection$query_point$y))
      ),
      tags$span(class = "knn-inspection-label", "Prediction"),
      tags$span(class = "knn-inspection-value", predicted_class),
      tags$span(class = "knn-inspection-label", vote_label),
      tags$span(class = "knn-inspection-value", vote_value)
    ),
    tags$table(
      class = "knn-neighbor-table",
      tags$thead(
        do.call(tags$tr, table_header_cells)
      ),
      tags$tbody(neighbor_rows)
    )
  )
}
