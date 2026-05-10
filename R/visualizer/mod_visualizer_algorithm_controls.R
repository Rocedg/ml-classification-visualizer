# R/visualizer/mod_visualizer_algorithm_controls.R
# Purpose:
#   Create the sidebar sections for algorithm selection and parameter setup.
#   This module also exposes the "Run Classifier" button click so the parent
#   visualizer module can train the chosen model at the right time.
#
# Functions:
#   - mod_visualizer_algorithm_controls_ui(): Build the algorithm and parameter sections.
#   - mod_visualizer_algorithm_controls_server(): Track selected algorithm,
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

mod_visualizer_algorithm_controls_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "sidebar-section wizard-step-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "2"),
        tags$span("Select Algorithm")
      ),
      uiOutput(ns("algorithm_cards_ui"))
    ),

    div(
      class = "sidebar-section wizard-step-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "3"),
        tags$span("Parameters")
      ),
      div(
        class = "parameter-panel",
        uiOutput(ns("parameter_controls_ui"))
      )
    ),

    div(
      class = "sidebar-section wizard-step-section run-step-section",
      div(
        class = "sidebar-section-header",
        div(class = "sidebar-step-pill", "4"),
        tags$span("Run")
      ),
      actionButton(
        inputId = ns("run_classifier_button"),
        label = "Run Classifier",
        class = "ml-button ml-button-primary ml-button-full",
        title = "Train the selected classifier and update the plot, metrics, and training views."
      )
    )
  )
}


mod_visualizer_algorithm_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    help_icon <- function(help_text) {
      tags$span(
        class = "help-tooltip",
        `aria-label` = help_text,
        "?"
      )
    }
    help_label <- function(label_text, help_text) {
      tagList(tags$span(label_text), help_icon(help_text))
    }

    # The selected key controls both the visible parameter inputs and the
    # training branch used when Run Classifier is clicked.
    selected_algorithm_key <- reactiveVal("logistic_regression")

    # Defaults are used when Shiny inputs have not initialized yet.
    default_logistic_learning_rate <- 0.12
    default_logistic_max_iter <- 60
    default_decision_threshold <- 0.50
    default_logistic_fit_intercept <- TRUE
    default_knn_k <- 5

    observeEvent(input$choose_logistic_regression, {
      selected_algorithm_key("logistic_regression")
    })

    observeEvent(input$choose_knn, {
      selected_algorithm_key("knn")
    })

    output$algorithm_cards_ui <- renderUI({
      # Cards are generated server-side so the active class can follow the
      # selected algorithm key.
      create_algorithm_card <- function(button_id, title_text, algorithm_key, tooltip_text, is_available = TRUE) {
        active_class <- if (identical(selected_algorithm_key(), algorithm_key)) "algorithm-selection-card is-active" else "algorithm-selection-card"

        card_contents <- tagList(
          div(
            class = "algorithm-card-title-row",
            tags$span(title_text),
            help_icon(tooltip_text)
          )
        )

        if (is_available) {
          actionLink(
            inputId = session$ns(button_id),
            label = card_contents,
            class = active_class,
            title = tooltip_text
          )
        } else {
          div(
            class = paste(active_class, "is-coming-soon"),
            title = tooltip_text,
            card_contents
          )
        }
      }

      div(
        class = "algorithm-card-stack",
        create_algorithm_card(
          button_id = "choose_logistic_regression",
          title_text = "Logistic Regression",
          algorithm_key = "logistic_regression",
          tooltip_text = "A linear classifier that estimates class probabilities and learns a decision boundary."
        ),
        create_algorithm_card(
          button_id = "choose_svm",
          title_text = "SVM",
          algorithm_key = "svm",
          tooltip_text = "Finds a separating boundary with the widest possible margin between classes.",
          is_available = FALSE
        ),
        create_algorithm_card(
          button_id = "choose_knn",
          title_text = "k-NN",
          algorithm_key = "knn",
          tooltip_text = "Classifies a point based on the majority class of its nearest neighbors."
        )
      )
    })

    output$parameter_controls_ui <- renderUI({
      # Parameter controls are algorithm-specific. The returned values are read
      # by mod_visualizer_server() only when Run Classifier is clicked.
      if (selected_algorithm_key() == "logistic_regression") {
        tagList(
          sliderInput(
            inputId = session$ns("logistic_learning_rate"),
            label = help_label(
              "Learning rate",
              "Controls how large each training update is. Higher values learn faster but may become unstable."
            ),
            min = 0.01,
            max = 1,
            value = 0.12,
            step = 0.01
          ),
          sliderInput(
            inputId = session$ns("logistic_max_iter"),
            label = help_label(
              "Iterations",
              "Maximum number of training steps shown in the visualization."
            ),
            min = 10,
            max = 100,
            value = 60,
            step = 10
          ),
          sliderInput(
            inputId = session$ns("decision_threshold"),
            label = help_label(
              "Threshold",
              "Probability cutoff used to assign a predicted class."
            ),
            min = 0.30,
            max = 0.70,
            value = 0.50,
            step = 0.01
          ),
          checkboxInput(
            inputId = session$ns("logistic_fit_intercept"),
            label = help_label(
              "Fit intercept",
              "Allows the decision boundary to shift by learning a bias term."
            ),
            value = TRUE
          )
        )
      } else if (selected_algorithm_key() == "knn") {
        tagList(
          sliderInput(
            inputId = session$ns("knn_k"),
            label = help_label(
              "k neighbors",
              "Number of nearest training points used to vote for the predicted class."
            ),
            min = 1,
            max = 25,
            value = 5,
            step = 1
          )
        )
      } else {
        tagList(
          div(
            class = "algorithm-placeholder-card",
            tags$span("Coming soon"),
            tags$p("Parameters for this algorithm will appear here when it is enabled.")
          )
        )
      }
    })

    output$run_helper_text <- renderText({
      if (selected_algorithm_key() == "logistic_regression") {
        "Run to update the boundary, metrics, and training views."
      } else if (selected_algorithm_key() == "knn") {
        "Run to update k-NN decision regions and metrics."
      } else {
        "This algorithm is coming soon. Choose Logistic Regression to run the classifier."
      }
    })

    list(
      selected_algorithm_key = reactive(selected_algorithm_key()),
      algorithm_parameters = reactive({
        if (selected_algorithm_key() == "logistic_regression") {
          logistic_learning_rate <- input$logistic_learning_rate
          logistic_max_iter <- input$logistic_max_iter
          decision_threshold <- input$decision_threshold
          logistic_fit_intercept <- input$logistic_fit_intercept

          if (is.null(logistic_learning_rate)) {
            logistic_learning_rate <- default_logistic_learning_rate
          }
          if (is.null(logistic_max_iter)) {
            logistic_max_iter <- default_logistic_max_iter
          }
          if (is.null(decision_threshold)) {
            decision_threshold <- default_decision_threshold
          }
          if (is.null(logistic_fit_intercept)) {
            logistic_fit_intercept <- default_logistic_fit_intercept
          }

          # Package UI values into the names expected by train_classification_model().
          list(
            logistic_learning_rate = logistic_learning_rate,
            logistic_max_iter = logistic_max_iter,
            decision_threshold = decision_threshold,
            logistic_fit_intercept = logistic_fit_intercept
          )
        } else if (selected_algorithm_key() == "knn") {
          knn_k <- input$knn_k

          if (is.null(knn_k)) {
            knn_k <- default_knn_k
          }

          list(
            knn_k = knn_k
          )
        } else {
          list()
        }
      }),
      run_model_clicks = reactive(input$run_classifier_button)
    )
  })
}
