# modules/home_module.R
# Purpose:
#   Build the landing page shown when the app first opens.
#   This page mirrors the reference image with a top hero layout,
#   feature cards underneath, and a footer at the bottom.
#
# Functions:
#   - home_module_ui(): Create the home page user interface.
#   - home_module_server(): Expose the hero button click to the main app.
#
# Inputs / Outputs:
#   Inputs:
#     - Click on the "Launch Visualizer" button
#   Outputs:
#     - Home page layout
#     - Reactive signal used to switch to the visualizer page

home_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "home-page",
      div(
        class = "content-container",
        div(
          class = "hero-section",
          div(
            class = "hero-text-column",
            tags$h1(
              class = "hero-title",
              HTML("Understand Machine<br>Learning, Step by Step.")
            ),
            tags$p(
              class = "hero-description",
              "An interactive educational tool to visualize how classification models learn from data in real time."
            ),
            actionButton(
              inputId = ns("launch_visualizer_button"),
              label = "Launch Visualizer",
              class = "ml-button ml-button-primary hero-cta-button"
            )
          ),
          div(
            class = "hero-visual-column",
            div(
              class = "hero-illustration-card",
              div(class = "hero-axis hero-axis-vertical"),
              div(class = "hero-axis hero-axis-horizontal"),
              div(class = "hero-trend-line"),
              div(class = "hero-dot dot-1 class-a"),
              div(class = "hero-dot dot-2 class-a"),
              div(class = "hero-dot dot-3 class-a"),
              div(class = "hero-dot dot-4 class-a"),
              div(class = "hero-dot dot-5 class-a"),
              div(class = "hero-dot dot-6 class-a"),
              div(class = "hero-dot dot-7 class-b"),
              div(class = "hero-dot dot-8 class-b"),
              div(class = "hero-dot dot-9 class-b"),
              div(class = "hero-dot dot-10 class-b"),
              div(class = "hero-dot dot-11 class-b"),
              div(class = "hero-dot dot-12 class-b")
            )
          )
        )
      ),

      div(
        class = "home-features-band",
        div(
          class = "content-container",
          div(class = "section-kicker", "Explore the Algorithms"),
          div(
            class = "feature-card-grid",
            div(
              class = "app-card feature-card",
              div(class = "feature-icon-circle", "P"),
              tags$h3("Draw Your Data"),
              tags$p("Create custom datasets by drawing points directly on the canvas or uploading your own CSV files.")
            ),
            div(
              class = "app-card feature-card",
              div(class = "feature-icon-circle", ">"),
              tags$h3("Step-by-Step Animation"),
              tags$p("Study how each classifier responds to different patterns and parameter choices in an interactive way.")
            ),
            div(
              class = "app-card feature-card",
              div(class = "feature-icon-circle", "M"),
              tags$h3("Real-time Metrics"),
              tags$p("Track accuracy, precision, recall, and F1 score while exploring decision boundaries.")
            )
          )
        )
      ),

      div(
        class = "home-footer",
        div(
          class = "content-container home-footer-inner",
          tags$span("(c) 2026 ML Visualizer. All rights reserved."),
          tags$span(class = "footer-mark", "ML")
        )
      )
    )
  )
}


home_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      launch_visualizer = reactive(input$launch_visualizer_button)
    )
  })
}
