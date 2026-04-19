# modules/algorithm_controls_module.R
# Purpose:
#   Create the sidebar sections for algorithm selection and parameter setup.
#   This module also exposes the "Run Classifier" button click so the parent
#   visualizer module can train the chosen model at the right time.
#
# Functions:
#   - algorithm_controls_module_ui(): Build sections 3 and 4 of the sidebar.
#   - algorithm_controls_module_server(): Track selected algorithm,
#     render dynamic parameter inputs, and expose run events.
#
# Inputs / Outputs:
#   Inputs:
#     - Algorithm card clicks
#     - Parameter sliders / dropdowns
#     - Run Classifier button
#   Outputs:
#     - Reactive selected algorithm key
#     - Reactive list of parameter values
#     - Reactive run button click count

algorithm_controls_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "sidebar-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "3"),
        tags$span("Select Algorithm")
      ),
      tags$p(
        class = "sidebar-helper-text",
        "Choose the classifier you want to run. Logistic Regression is highlighted first because it is often the easiest model to interpret."
      ),
      uiOutput(ns("algorithm_cards_ui"))
    ),

    div(
      class = "sidebar-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "4"),
        tags$span("Configure Parameters")
      ),
      tags$p(
        class = "sidebar-helper-text",
        "Each algorithm shows only the controls that matter for it."
      ),
      uiOutput(ns("parameter_controls_ui")),
      actionButton(
        inputId = ns("run_classifier_button"),
        label = "Run Classifier",
        class = "ml-button ml-button-primary ml-button-full"
      ),
      div(class = "sidebar-run-note", "Complete steps above to run"),
      div(
        class = "run-helper-text",
        textOutput(ns("run_helper_text"))
      )
    )
  )
}


algorithm_controls_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected_algorithm_key <- reactiveVal("logistic_regression")

    observeEvent(input$choose_logistic_regression, {
      selected_algorithm_key("logistic_regression")
    })

    observeEvent(input$choose_svm, {
      selected_algorithm_key("svm")
    })

    observeEvent(input$choose_knn, {
      selected_algorithm_key("knn")
    })

    output$algorithm_cards_ui <- renderUI({
      create_algorithm_card <- function(button_id, title_text, description_text, helper_badge, algorithm_key) {
        active_class <- if (identical(selected_algorithm_key(), algorithm_key)) "algorithm-selection-card is-active" else "algorithm-selection-card"

        actionLink(
          inputId = session$ns(button_id),
          label = HTML(
            paste0(
              "<div class='algorithm-card-title-row'>",
              "<span>", title_text, "</span>",
              "<span class='inline-status-badge subtle'>", helper_badge, "</span>",
              "</div>",
              "<p>", description_text, "</p>"
            )
          ),
          class = active_class
        )
      }

      div(
        class = "algorithm-card-stack",
        create_algorithm_card(
          button_id = "choose_logistic_regression",
          title_text = "Logistic Regression",
          description_text = "Fits a probability-based linear boundary that is easy to interpret.",
          helper_badge = "Recommended",
          algorithm_key = "logistic_regression"
        ),
        create_algorithm_card(
          button_id = "choose_svm",
          title_text = "SVM",
          description_text = "Learns a boundary with margin maximization and can model nonlinear regions.",
          helper_badge = "Flexible",
          algorithm_key = "svm"
        ),
        create_algorithm_card(
          button_id = "choose_knn",
          title_text = "k-NN",
          description_text = "Classifies by local neighbor voting and is useful for comparison.",
          helper_badge = "Optional",
          algorithm_key = "knn"
        )
      )
    })

    output$parameter_controls_ui <- renderUI({
      if (selected_algorithm_key() == "logistic_regression") {
        tagList(
          sliderInput(
            inputId = session$ns("decision_threshold"),
            label = "Decision Threshold",
            min = 0.30,
            max = 0.70,
            value = 0.50,
            step = 0.01
          ),
          tags$p(
            class = "parameter-note",
            "The threshold decides when a predicted probability becomes Class B."
          )
        )
      } else if (selected_algorithm_key() == "svm") {
        tagList(
          selectInput(
            inputId = session$ns("svm_kernel"),
            label = "Kernel",
            choices = c("radial", "linear"),
            selected = "radial"
          ),
          sliderInput(
            inputId = session$ns("svm_cost"),
            label = "Cost",
            min = 0.1,
            max = 5,
            value = 1,
            step = 0.1
          ),
          sliderInput(
            inputId = session$ns("svm_gamma"),
            label = "Gamma",
            min = 0.05,
            max = 2,
            value = 0.35,
            step = 0.05
          ),
          tags$p(
            class = "parameter-note",
            "Higher cost penalizes mistakes more strongly. Higher gamma makes the boundary more local and more curved."
          )
        )
      } else {
        tagList(
          sliderInput(
            inputId = session$ns("knn_neighbors"),
            label = "Number of Neighbors (k)",
            min = 1,
            max = 25,
            value = 7,
            step = 2
          ),
          tags$p(
            class = "parameter-note",
            "Small k creates more detailed local regions. Larger k creates smoother, more stable regions."
          )
        )
      }
    })

    output$run_helper_text <- renderText({
      if (selected_algorithm_key() == "logistic_regression") {
        "Run the classifier to draw a linear probability boundary and compute the training metrics."
      } else if (selected_algorithm_key() == "svm") {
        "Run the classifier to visualize the SVM decision regions and compare them with Logistic Regression."
      } else {
        "Run the classifier to see how local neighbor voting shapes the classification regions."
      }
    })

    list(
      selected_algorithm_key = reactive(selected_algorithm_key()),
      algorithm_parameters = reactive({
        if (selected_algorithm_key() == "logistic_regression") {
          list(
            decision_threshold = input$decision_threshold
          )
        } else if (selected_algorithm_key() == "svm") {
          list(
            svm_kernel = input$svm_kernel,
            svm_cost = input$svm_cost,
            svm_gamma = input$svm_gamma
          )
        } else {
          list(
            knn_neighbors = input$knn_neighbors
          )
        }
      }),
      run_model_clicks = reactive(input$run_classifier_button)
    )
  })
}
