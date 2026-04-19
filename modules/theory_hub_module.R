# modules/theory_hub_module.R
# Purpose:
#   Provide a beginner-friendly learning page that explains the supported
#   classification algorithms and the overall workflow in plain language.
#
# Functions:
#   - theory_hub_module_ui(): Create the Theory Hub page UI.
#   - theory_hub_module_server(): Placeholder server for future expansion.
#
# Inputs / Outputs:
#   Inputs:
#     - None currently
#   Outputs:
#     - Static educational content cards

theory_hub_module_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "simple-page",
    div(
      class = "content-container theory-page",
      div(
        class = "page-hero-block",
        tags$h1("Theory Hub"),
        tags$p("Learn the ideas behind the classifiers used in ML Visualizer. Each section is written to be approachable for beginners.")
      ),
      div(
        class = "theory-card-grid",
        div(
          class = "app-card theory-card",
          tags$h3("Logistic Regression"),
          tags$p("Logistic Regression estimates the probability that a point belongs to a class. It works best when the classes can be separated with a mostly straight boundary."),
          tags$ul(
            tags$li("Output: a probability between 0 and 1"),
            tags$li("Decision rule: assign a class using a threshold"),
            tags$li("Good for simple, interpretable boundaries")
          )
        ),
        div(
          class = "app-card theory-card",
          tags$h3("Support Vector Machine"),
          tags$p("SVM tries to place a boundary that separates the classes as confidently as possible. With different kernels, it can learn straight or curved boundaries."),
          tags$ul(
            tags$li("Focus: separation margin"),
            tags$li("Important parameters: cost and gamma"),
            tags$li("Useful for flexible nonlinear boundaries")
          )
        ),
        div(
          class = "app-card theory-card",
          tags$h3("k-Nearest Neighbors"),
          tags$p("k-NN classifies a new point by looking at the classes of nearby points. It is intuitive because predictions depend on local neighborhoods."),
          tags$ul(
            tags$li("No training equation to fit"),
            tags$li("Main parameter: number of neighbors"),
            tags$li("Great for comparing local voting behavior")
          )
        ),
        div(
          class = "app-card theory-card theory-card-wide",
          tags$h3("How to Use the Visualizer"),
          tags$ol(
            tags$li("Pick a 2D dataset or upload your own CSV."),
            tags$li("Optionally draw extra Class A or Class B points on the plot."),
            tags$li("Choose a classification algorithm."),
            tags$li("Adjust the parameters in the sidebar."),
            tags$li("Run the classifier and study the decision regions, metrics, and raw data.")
          )
        )
      )
    )
  )
}


theory_hub_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
