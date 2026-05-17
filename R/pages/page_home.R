# R/pages/page_home.R
# Purpose:
#   Build the landing page shown when the app first opens.
#   This page contains the main hero section, a professional ML preview card,
#   feature cards, and a footer.

page_home_ui <- function(id) {
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
              style = "
                width: min(480px, 100%);
                background: #ffffff;
                border: 1px solid rgba(121, 201, 183, 0.75);
                border-radius: 18px;
                box-shadow: 0 18px 42px rgba(36, 59, 87, 0.14);
                overflow: hidden;
              ",
              
              div(
                style = "
                  height: 48px;
                  padding: 0 18px;
                  display: flex;
                  align-items: center;
                  gap: 8px;
                  border-bottom: 1px solid #e5ebf1;
                  background: #ffffff;
                ",
                
                span(
                  style = "
                    width: 9px;
                    height: 9px;
                    border-radius: 50%;
                    background: #ff8b3d;
                    display: inline-block;
                  "
                ),
                span(
                  style = "
                    width: 9px;
                    height: 9px;
                    border-radius: 50%;
                    background: #f5c04f;
                    display: inline-block;
                  "
                ),
                span(
                  style = "
                    width: 9px;
                    height: 9px;
                    border-radius: 50%;
                    background: #79c9b7;
                    display: inline-block;
                  "
                ),
                
                span(
                  style = "
                    margin-left: auto;
                    color: #6d8196;
                    font-size: 12px;
                    font-weight: 800;
                    letter-spacing: 0.04em;
                    text-transform: uppercase;
                  ",
                  "Classification Preview"
                )
              ),
              
              div(
                style = "padding: 22px;",
                
                tags$svg(
                  width = "100%",
                  height = "270",
                  viewBox = "0 0 420 270",
                  style = "
                    background: #fbfdff;
                    border: 1px solid #e8eef3;
                    border-radius: 16px;
                    display: block;
                  ",
                  
                  tags$defs(
                    tags$linearGradient(
                      id = "decisionGradient",
                      x1 = "0%",
                      y1 = "100%",
                      x2 = "100%",
                      y2 = "0%",
                      tags$stop(offset = "0%", `stop-color` = "#5a95ff"),
                      tags$stop(offset = "100%", `stop-color` = "#ff8b3d")
                    )
                  ),
                  
                  tags$polygon(
                    points = "0,270 0,0 170,0 420,270",
                    fill = "rgba(90,149,255,0.12)"
                  ),
                  tags$polygon(
                    points = "170,0 420,0 420,270",
                    fill = "rgba(255,139,61,0.14)"
                  ),
                  
                  tags$line(
                    x1 = "105", y1 = "0",
                    x2 = "105", y2 = "270",
                    stroke = "#e8eef3",
                    `stroke-width` = "1"
                  ),
                  tags$line(
                    x1 = "210", y1 = "0",
                    x2 = "210", y2 = "270",
                    stroke = "#e8eef3",
                    `stroke-width` = "1"
                  ),
                  tags$line(
                    x1 = "315", y1 = "0",
                    x2 = "315", y2 = "270",
                    stroke = "#e8eef3",
                    `stroke-width` = "1"
                  ),
                  tags$line(
                    x1 = "0", y1 = "90",
                    x2 = "420", y2 = "90",
                    stroke = "#e8eef3",
                    `stroke-width` = "1"
                  ),
                  tags$line(
                    x1 = "0", y1 = "180",
                    x2 = "420", y2 = "180",
                    stroke = "#e8eef3",
                    `stroke-width` = "1"
                  ),
                  
                  tags$line(
                    x1 = "45",
                    y1 = "215",
                    x2 = "370",
                    y2 = "55",
                    stroke = "url(#decisionGradient)",
                    `stroke-width` = "5",
                    `stroke-linecap` = "round"
                  ),
                  
                  tags$circle(
                    cx = "72", cy = "205", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "100", cy = "188", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "130", cy = "175", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "158", cy = "158", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "188", cy = "142", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "215", cy = "128", r = "9",
                    fill = "#5a95ff",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  
                  tags$circle(
                    cx = "235", cy = "117", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "260", cy = "103", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "285", cy = "88", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "315", cy = "73", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "340", cy = "60", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  ),
                  tags$circle(
                    cx = "360", cy = "48", r = "9",
                    fill = "#ff8b3d",
                    stroke = "#ffffff",
                    `stroke-width` = "3"
                  )
                )
              ),
              
              div(
                style = "
                  display: grid;
                  grid-template-columns: repeat(3, 1fr);
                  gap: 12px;
                  padding: 0 22px 22px;
                ",
                
                div(
                  style = "
                    padding: 12px;
                    background: #f8fbfd;
                    border: 1px solid #e5ebf1;
                    border-radius: 14px;
                    text-align: center;
                  ",
                  span(
                    style = "
                      display: block;
                      color: #6d8196;
                      font-size: 10px;
                      font-weight: 800;
                      text-transform: uppercase;
                    ",
                    "Accuracy"
                  ),
                  span(
                    style = "
                      display: block;
                      color: #243b57;
                      font-size: 20px;
                      font-weight: 800;
                    ",
                    "92%"
                  )
                ),
                
                div(
                  style = "
                    padding: 12px;
                    background: #f8fbfd;
                    border: 1px solid #e5ebf1;
                    border-radius: 14px;
                    text-align: center;
                  ",
                  span(
                    style = "
                      display: block;
                      color: #6d8196;
                      font-size: 10px;
                      font-weight: 800;
                      text-transform: uppercase;
                    ",
                    "F1 Score"
                  ),
                  span(
                    style = "
                      display: block;
                      color: #243b57;
                      font-size: 20px;
                      font-weight: 800;
                    ",
                    "0.89"
                  )
                ),
                
                div(
                  style = "
                    padding: 12px;
                    background: #f8fbfd;
                    border: 1px solid #e5ebf1;
                    border-radius: 14px;
                    text-align: center;
                  ",
                  span(
                    style = "
                      display: block;
                      color: #6d8196;
                      font-size: 10px;
                      font-weight: 800;
                      text-transform: uppercase;
                    ",
                    "Model"
                  ),
                  span(
                    style = "
                      display: block;
                      color: #243b57;
                      font-size: 20px;
                      font-weight: 800;
                    ",
                    "LR"
                  )
                )
              )
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
          tags$span("© 2026 ML Visualizer. All rights reserved."),
          tags$span(class = "footer-mark", "ML")
        )
      )
    )
  )
}


page_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      launch_visualizer = reactive(input$launch_visualizer_button)
    )
  })
}
