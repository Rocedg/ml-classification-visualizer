# R/visualizer/mod_visualizer_training_insights.R
# Purpose:
#   Build the Training insights tab UI.

mod_visualizer_training_insights_ui <- function(id) {
  ns <- NS(id)
  help_icon <- function(help_text) {
    tags$span(
      class = "help-tooltip",
      `aria-label` = help_text,
      "?"
    )
  }

  div(
    class = "training-insights-tab-layout",
    div(
      class = "training-insights-header",
      tags$h3(tags$span("Training insights"), help_icon("Shows model-specific training details.")),
      tags$p(textOutput(ns("training_insights_subtitle"), inline = TRUE))
    ),
    conditionalPanel(
      condition = paste0("output['", ns("training_insights_algorithm"), "'] == 'logistic_regression'"),
      div(
        class = "training-insights-grid",
        div(
          class = "plot-canvas-shell diagnostic-plot-shell",
          div(
            class = "diagnostic-section-header",
            tags$span("Loss curve"),
            help_icon("Shows how prediction error changes over training iterations.")
          ),
          plotOutput(
            outputId = ns("iteration_metric_plot"),
            height = "300px"
          )
        ),
        div(
          class = "app-card theory-summary-card diagnostic-plot-shell parameter-diagnostic-card",
          div(
            class = "plot-top-status-row",
            div(
              class = "status-chip status-chip-primary",
              textOutput(ns("parameter_diagnostic_title"), inline = TRUE),
              help_icon("Shows how the learned model parameters move during optimization.")
            )
          ),
          conditionalPanel(
            condition = paste0("output['", ns("show_3d_parameter_diagnostic"), "'] == 'true'"),
            plotly::plotlyOutput(
              outputId = ns("parameter_trajectory_3d_plot"),
              height = "320px"
            )
          ),
          conditionalPanel(
            condition = paste0("output['", ns("show_3d_parameter_diagnostic"), "'] == 'false'"),
            plotOutput(
              outputId = ns("bias_fixed_loss_landscape_plot"),
              height = "320px"
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = paste0("output['", ns("training_insights_algorithm"), "'] == 'knn'"),
      div(
        class = "app-card theory-summary-card",
        tags$h3("How k-NN predicts"),
        tags$p("k-NN does not learn parameters through iterations. It classifies each point by looking at the k nearest training points and using majority vote."),
        tags$p("Smaller k can create more flexible boundaries; larger k usually creates smoother boundaries.")
      )
    ),
    conditionalPanel(
      condition = paste0("output['", ns("training_insights_algorithm"), "'] == 'svm'"),
      div(
        class = "app-card theory-summary-card",
        tags$h3("How SVM decides"),
        tags$p("SVM chooses a decision boundary by maximizing the margin between classes. Support vectors are the influential training points that define that margin."),
        tags$p("This visualizer does not animate SVM optimizer steps. Classical SVM training is a constrained optimization problem, so this tab focuses on the final boundary, support vectors, and parameter effects."),
        tags$p("C controls the penalty for margin violations. Kernels control the boundary shape: Linear is straight, RBF creates flexible local regions, and Polynomial creates curved boundaries controlled by degree."),
        uiOutput(ns("svm_training_summary_ui")),
        tags$h4("SVM decision function"),
        tags$p("The surface shows the SVM decision score across the feature space. The decision boundary is where the score crosses 0."),
        plotly::plotlyOutput(
          outputId = ns("svm_decision_surface_plot"),
          height = "320px"
        )
      )
    )
  )
}
