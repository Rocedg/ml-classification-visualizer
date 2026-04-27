# modules/model_theory_panel_module.R
# Purpose:
#   Display a beginner-friendly explanation of the currently selected model.
#   This tab combines conceptual explanations with the latest metric values.
#
# Functions:
#   - model_theory_panel_module_ui(): Build the Model Theory tab layout.
#   - model_theory_panel_module_server(): Render algorithm-specific content.
#
# Inputs / Outputs:
#   Inputs:
#     - Reactive selected algorithm key
#     - Reactive trained model bundle
#   Outputs:
#     - Styled explanatory content for the selected model


model_theory_panel_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "theory-panel-layout",
    uiOutput(ns("model_theory_content_ui"))
  )
}


model_theory_panel_module_server <- function(id, selected_algorithm_key, trained_model_bundle) {
  moduleServer(id, function(input, output, session) {

    #  Configuración centralizada por modelo
    model_theory_content <- list(

      logistic_regression = list(
        title = "Logistic Regression",
        explanation = "Logistic Regression estimates the probability that a point belongs to Class B. The decision boundary appears where that probability crosses the threshold you selected.",
        strengths = c(
          "Easy to interpret",
          "Fast baseline model",
          "Works well for roughly linear separation"
        ),
        parameter_note = "Main parameter in this app: decision threshold."
      ),

      svm = list(
        title = "Support Vector Machine",
        explanation = "SVM searches for a separating boundary that maximizes the margin between classes. With a radial kernel, the boundary can bend around more complex shapes.",
        strengths = c(
          "Can learn nonlinear boundaries",
          "Often performs well on complex shapes",
          "Margin-based decision rule"
        ),
        parameter_note = "Main parameters in this app: kernel, cost, and gamma."
      ),

      knn = list(
        title = "k-Nearest Neighbors",
        explanation = "k-NN does not fit a single equation. Instead, it looks at nearby labeled points and uses their votes to decide the class for each location.",
        strengths = c(
          "Very intuitive",
          "Responds to local structure",
          "Useful for comparing neighborhood effects"
        ),
        parameter_note = "Main parameter in this app: number of neighbors (k)."
      )
    )

    output$model_theory_content_ui <- renderUI({

      algorithm_key <- selected_algorithm_key()
      model_results <- tryCatch(trained_model_bundle(), error = function(error_object) NULL)

      #  Obtener configuración del modelo
      model_info <- model_theory_content[[algorithm_key]]

      # fallback si no existe
      if (is.null(model_info)) {
        return(tags$p("No theory available for this model."))
      }

      metrics_block <- if (is.null(model_results)) {
        div(
          class = "app-card theory-summary-card",
          tags$h4("Current Run Summary"),
          tags$p("Run the classifier from the sidebar to populate this summary with live metrics.")
        )
      } else {
        div(
          class = "app-card theory-summary-card",
          tags$h4("Current Run Summary"),
          tags$p(paste("Algorithm:", model_results$algorithm_label)),
          tags$p(paste("Accuracy:", model_results$metrics$accuracy)),
          tags$p(paste("Precision:", model_results$metrics$precision)),
          tags$p(paste("Recall:", model_results$metrics$recall)),
          tags$p(paste("F1 Score:", model_results$metrics$f1_score))
        )
      }

      div(
        class = "theory-panel-grid",
        div(
          class = "app-card theory-detail-card",
          tags$h3(model_info$title),
          tags$p(model_info$explanation),
          tags$h4("Why it helps in this app"),
          tags$ul(lapply(model_info$strengths, tags$li)),
          tags$h4("Parameter focus"),
          tags$p(model_info$parameter_note)
        ),
        metrics_block
      )
    })
  })
}
