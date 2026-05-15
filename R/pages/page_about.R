# R/pages/page_about.R
# Purpose:
#   Present a simple "About Us" page for the project.
#   This page explains the goal of the tool and the kind of learner it serves.
#
# Functions:
#   - page_about_ui(): Create the About Us page.
#   - page_about_server(): Placeholder server for future content.
#
# Inputs / Outputs:
#   Inputs:
#     - None currently
#   Outputs:
#     - Static informational content

page_about_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "simple-page",
    div(
      class = "content-container about-page",
      div(
        class = "page-hero-block",
        tags$h1("About Us"),
        tags$p("ML Visualizer was designed as an educational Shiny application that turns abstract machine learning ideas into something you can see, test, and understand.")
      ),
      div(
        class = "about-card-grid",
        div(
          class = "app-card about-card",
          tags$h3("Our Goal"),
          tags$p("We want beginners to understand how classification models behave, not just memorize definitions. The app focuses on visual intuition first.")
        ),
        div(
          class = "app-card about-card",
          tags$h3("Why 2D Data"),
          tags$p("Two-dimensional datasets make the decision boundary visible. That makes it much easier to connect model predictions with the original data.")
        ),
        div(
          class = "app-card about-card",
          tags$h3("Built for Learning"),
          tags$p("The project uses clear names, modular files, and extensive comments so new R programmers can read the code and keep building confidently.")
        ),
        
        div(
          class = "app-card about-card project-info-card",
          tags$h3("Project Information"),
          tags$p(
            tags$strong("Team: "),
            "Alex Garcia , Edgar Roca, Jordi Beascoechea."
          ),
          tags$p(
            tags$strong("Subject: "),
            "Projecte 2."
          ),
          tags$p(
            tags$strong("University: "),
            "Universitat Politècnica de Catalunya (UPC)."
          ),
          tags$p(
            tags$strong("GitHub repository: "),
            tags$a(
              href = "https://github.com/Rocedg/ml-classification-visualizer",
              target = "_blank",
              "ml-classification-visualizer"
            )
          ),
          tags$p(
            tags$strong("Server: "),
            "Local Shiny server using shiny::runApp()."
          )
        )
      )
    )
  )
}


page_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
