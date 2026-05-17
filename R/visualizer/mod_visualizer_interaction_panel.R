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
      ),
      conditionalPanel(
        condition = paste0("output['", ns("show_svm_summary_panel"), "'] == 'true'"),
        uiOutput(ns("svm_summary_ui"))
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


calculate_knn_inspection_confidence <- function(inspection, voting_method) {
  predicted_class <- inspection$predicted_class

  if (is.null(predicted_class) || length(predicted_class) != 1 || is.na(predicted_class)) {
    return(NA_real_)
  }

  if (identical(voting_method, "distance_weighted")) {
    weighted_votes <- inspection$weighted_votes

    if (is.null(weighted_votes) || is.null(weighted_votes[[predicted_class]])) {
      return(NA_real_)
    }

    total_weight <- sum(weighted_votes, na.rm = TRUE)

    if (!is.finite(total_weight) || total_weight <= 0) {
      return(NA_real_)
    }

    return(as.numeric(weighted_votes[[predicted_class]]) / total_weight)
  }

  vote_counts <- inspection$vote_counts

  if (is.null(vote_counts) || is.null(vote_counts[[predicted_class]])) {
    return(NA_real_)
  }

  total_votes <- inspection$effective_k

  if (is.null(total_votes) || length(total_votes) != 1 || is.na(total_votes) || total_votes <= 0) {
    total_votes <- sum(vote_counts, na.rm = TRUE)
  }

  if (!is.finite(total_votes) || total_votes <= 0) {
    return(NA_real_)
  }

  as.numeric(vote_counts[[predicted_class]]) / total_votes
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
  is_weighted_voting <- identical(voting_method, "distance_weighted")
  show_weight_column <- is_weighted_voting && "weight" %in% names(neighbors)
  confidence_value <- calculate_knn_inspection_confidence(inspection, voting_method)

  if (is_weighted_voting) {
    weighted_votes <- inspection$weighted_votes
    class_a_weighted_vote <- if (is.null(weighted_votes[["Class A"]])) NA_real_ else weighted_votes[["Class A"]]
    class_b_weighted_vote <- if (is.null(weighted_votes[["Class B"]])) NA_real_ else weighted_votes[["Class B"]]
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
      tags$span(class = "knn-inspection-label", "Confidence"),
      tags$span(class = "knn-inspection-value", format_inspection_number(confidence_value)),
      tags$span(class = "knn-inspection-label", vote_label),
      tags$span(class = "knn-inspection-value", vote_value)
    ),
    tags$div(class = "knn-neighbor-table-title", "Nearest neighbors"),
    tags$table(
      class = "knn-neighbor-table",
      tags$thead(
        do.call(tags$tr, table_header_cells)
      ),
      tags$tbody(neighbor_rows)
    )
  )
}


visualizer_svm_margin_panel_ui <- function(selected_algorithm, model_results) {
  if (!identical(selected_algorithm, "svm")) {
    return(NULL)
  }

  if (is.null(model_results) || !identical(model_results$algorithm_key, "svm")) {
    return(div(
      class = "knn-inspection-panel svm-summary-panel",
      tags$h4("SVM optimization view"),
      tags$p("Run SVM to inspect the final boundary, support vectors, and score contours.")
    ))
  }

  summary_row <- function(label_text, value_text) {
    tagList(
      tags$span(class = "knn-inspection-label", label_text),
      tags$span(class = "knn-inspection-value", value_text)
    )
  }

  div(
    class = "knn-inspection-panel svm-summary-panel",
    tags$h4("SVM optimization view"),
    tags$p(
      "SVM is not shown here as a gradient descent animation. Classical SVM training is usually formulated as a constrained optimization problem: it searches for a boundary that maximizes the margin while allowing controlled violations."
    ),
    tags$p(
      "That optimization process is mathematically more complex and outside this visualizer's scope. Instead, this view focuses on the final boundary, support vectors, score contours, and how parameters affect the model."
    ),
    div(
      class = "knn-inspection-grid",
      summary_row("Boundary", "score = 0"),
      summary_row("Score contours", "-1 and +1 when available"),
      summary_row("Support vectors", "highlighted rings")
    )
  )
}
