# app.R
# Purpose:
#   Main entry point for the "ML Visualizer" Shiny application.
#   This file loads packages, sources helper and module files, creates the
#   top-level navigation, and connects the page modules together.
#
# Inputs / Outputs:
#   Inputs:
#     - Navigation clicks from the top navbar
#     - "Launch Visualizer" click from the home page
#   Outputs:
#     - Full multi-page Shiny application UI
#     - Shared helper functions used by the modules

library(shiny)
library(ggplot2)


# ---------------------------- Helper Source Files -----------------------------

# Source helper files before modules so their functions are available everywhere.
helper_files <- c(
  "R/data_helpers.R",
  "R/metrics_helpers.R",
  "R/model_training_helpers.R"
)

for (helper_file in helper_files) {
  sys.source(helper_file, envir = environment())
}


# ----------------------------- Module Source Files ----------------------------

module_files <- c(
  "modules/home_module.R",
  "modules/visualizer_module.R",
  "modules/theory_hub_module.R",
  "modules/about_module.R",
  "modules/dataset_controls_module.R",
  "modules/algorithm_controls_module.R",
  "modules/plot_panel_module.R",
  "modules/raw_data_module.R",
  "modules/model_theory_panel_module.R"
)

for (module_file in module_files) {
  sys.source(module_file, envir = environment())
}


# ------------------------------- Application UI -------------------------------

ui <- fluidPage(
  tags$head(
    tags$title("ML Visualizer"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700;800&display=swap"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(
    class = "page-shell",
    uiOutput("top_navbar"),
    div(
      class = "page-content-shell",
      tabsetPanel(
        id = "main_navigation",
        type = "hidden",
        selected = "home",
        tabPanel("home", home_module_ui("home_page")),
        tabPanel("visualizer", visualizer_module_ui("visualizer_page")),
        tabPanel("theory_hub", theory_hub_module_ui("theory_hub_page")),
        tabPanel("about_us", about_module_ui("about_page"))
      )
    )
  )
)


# ----------------------------- Application Server ----------------------------

server <- function(input, output, session) {
  current_page <- reactiveVal("home")

  output$top_navbar <- renderUI({
    nav_link <- function(link_id, label, page_value) {
      active_class <- if (identical(current_page(), page_value)) "top-nav-link is-active" else "top-nav-link"
      actionLink(inputId = link_id, label = label, class = active_class)
    }

    div(
      class = "top-navbar",
      div(class = "top-navbar-inner",
        div(class = "brand-mark", "ML Visualizer"),
        div(class = "top-navbar-links",
          nav_link("nav_home", "Home", "home"),
          nav_link("nav_visualizer", "The Visualizer", "visualizer"),
          nav_link("nav_theory_hub", "Theory Hub", "theory_hub"),
          nav_link("nav_about_us", "About Us", "about_us")
        )
      )
    )
  })

  home_page <- home_module_server("home_page")
  visualizer_module_server("visualizer_page")
  theory_hub_module_server("theory_hub_page")
  about_module_server("about_page")

  observeEvent(input$nav_home, {
    current_page("home")
  })

  observeEvent(input$nav_visualizer, {
    current_page("visualizer")
  })

  observeEvent(input$nav_theory_hub, {
    current_page("theory_hub")
  })

  observeEvent(input$nav_about_us, {
    current_page("about_us")
  })

  observeEvent(home_page$launch_visualizer(), {
    current_page("visualizer")
  })

  observe({
    updateTabsetPanel(session, inputId = "main_navigation", selected = current_page())
  })
}

shinyApp(ui = ui, server = server)
