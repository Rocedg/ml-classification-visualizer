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
    default_decision_threshold <- 0.50
    default_logistic_c <- 1
    default_logistic_l1_ratio <- 0
    default_logistic_fit_intercept <- TRUE
    default_logistic_max_iter <- 60
    default_logistic_tol <- 0.001

    observeEvent(input$choose_logistic_regression, {
      selected_algorithm_key("logistic_regression")
    })

    output$algorithm_cards_ui <- renderUI({
      create_algorithm_card <- function(button_id, title_text, description_text, helper_badge, algorithm_key, is_available = TRUE) {
        active_class <- if (identical(selected_algorithm_key(), algorithm_key)) "algorithm-selection-card is-active" else "algorithm-selection-card"

        card_contents <- HTML(
          paste0(
            "<div class='algorithm-card-title-row'>",
            "<span>", title_text, "</span>",
            "<span class='inline-status-badge subtle'>", helper_badge, "</span>",
            "</div>",
            "<p>", description_text, "</p>"
          )
        )

        if (is_available) {
          actionLink(
            inputId = session$ns(button_id),
            label = card_contents,
            class = active_class
          )
        } else {
          div(
            class = paste(active_class, "is-coming-soon"),
            card_contents
          )
        }
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
          helper_badge = "Coming soon",
          algorithm_key = "svm",
          is_available = FALSE
        ),
        create_algorithm_card(
          button_id = "choose_knn",
          title_text = "k-NN",
          description_text = "Classifies by local neighbor voting and is useful for comparison.",
          helper_badge = "Coming soon",
          algorithm_key = "knn",
          is_available = FALSE
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
          sliderInput(
            inputId = session$ns("logistic_c"),
            label = "Regularization Strength (C)",
            min = 0.1,
            max = 10,
            value = 1,
            step = 0.1
          ),
          sliderInput(
            inputId = session$ns("logistic_l1_ratio"),
            label = "Regularization Mix",
            min = 0,
            max = 1,
            value = 0,
            step = 0.05
          ),
          checkboxInput(
            inputId = session$ns("logistic_fit_intercept"),
            label = "Fit Intercept",
            value = TRUE
          ),
          sliderInput(
            inputId = session$ns("logistic_max_iter"),
            label = "Max Iterations",
            min = 10,
            max = 200,
            value = 60,
            step = 10
          ),
          sliderInput(
            inputId = session$ns("logistic_tol"),
            label = "Stopping Tolerance",
            min = 0.0001,
            max = 0.01,
            value = 0.001,
            step = 0.0001
          ),
          tags$p(
            class = "parameter-note",
            "The threshold decides when a predicted probability becomes Class B."
          ),
          tags$p(
            class = "parameter-note",
            "C controls regularization: smaller C means stronger simplification. The mix slider moves from L2-style smooth shrinkage to L1-style stronger feature selection."
          )
        )
      } else {
        tagList(
          tags$p(
            class = "parameter-note",
            "This algorithm is coming soon. Logistic Regression is the only classifier available right now."
          )
        )
      }
    })

    output$run_helper_text <- renderText({
      if (selected_algorithm_key() == "logistic_regression") {
        "Run the classifier to draw a linear probability boundary and compute the training metrics."
      } else {
        "This algorithm is coming soon. Choose Logistic Regression to run the classifier."
      }
    })

    list(
      selected_algorithm_key = reactive(selected_algorithm_key()),
      algorithm_parameters = reactive({
        if (selected_algorithm_key() == "logistic_regression") {
          decision_threshold <- input$decision_threshold
          logistic_c <- input$logistic_c
          logistic_l1_ratio <- input$logistic_l1_ratio
          logistic_fit_intercept <- input$logistic_fit_intercept
          logistic_max_iter <- input$logistic_max_iter
          logistic_tol <- input$logistic_tol

          if (is.null(decision_threshold)) {
            decision_threshold <- default_decision_threshold
          }
          if (is.null(logistic_c)) {
            logistic_c <- default_logistic_c
          }
          if (is.null(logistic_l1_ratio)) {
            logistic_l1_ratio <- default_logistic_l1_ratio
          }
          if (is.null(logistic_fit_intercept)) {
            logistic_fit_intercept <- default_logistic_fit_intercept
          }
          if (is.null(logistic_max_iter)) {
            logistic_max_iter <- default_logistic_max_iter
          }
          if (is.null(logistic_tol)) {
            logistic_tol <- default_logistic_tol
          }

          list(
            decision_threshold = decision_threshold,
            logistic_c = logistic_c,
            logistic_l1_ratio = logistic_l1_ratio,
            logistic_fit_intercept = logistic_fit_intercept,
            logistic_max_iter = logistic_max_iter,
            logistic_tol = logistic_tol
          )
        } else {
          list()
        }
      }),
      run_model_clicks = reactive(input$run_classifier_button)
    )
  })
}
