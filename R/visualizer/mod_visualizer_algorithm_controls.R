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
        tags$span("Model")
      ),
      uiOutput(ns("algorithm_cards_ui")),
      div(
        class = "run-defaults-card",
        tags$p("Use the recommended starting parameters for this model."),
        actionButton(
          inputId = ns("run_defaults_button"),
          label = "Run with defaults",
          class = "ml-button ml-button-secondary ml-button-full",
          title = "Run the selected model using recommended starting parameters."
        )
      )
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
        tags$p(class = "sidebar-helper-text", "Tune the selected model, or keep the defaults for a quick first run."),
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
      tags$p(class = "run-helper-text", textOutput(ns("run_helper_text"), inline = TRUE)),
      actionButton(
        inputId = ns("run_classifier_button"),
        label = "Run model",
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
    default_knn_distance_metric <- "euclidean"
    default_knn_voting_method <- "uniform"
    default_svm_cost <- 1
    default_svm_gamma <- 0.5
    default_svm_degree <- 3

    observeEvent(input$choose_logistic_regression, {
      selected_algorithm_key("logistic_regression")
    })

    observeEvent(input$choose_knn, {
      selected_algorithm_key("knn")
    })

    observeEvent(input$choose_svm, {
      selected_algorithm_key("svm")
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
          algorithm_key = "logistic_regression",
          tooltip_text = "A linear classifier that estimates class probabilities and learns a decision boundary."
        ),
        create_algorithm_card(
          button_id = "choose_svm",
          title_text = "SVM",
          algorithm_key = "svm",
          tooltip_text = "Finds a decision boundary that maximizes the margin between classes. The closest training points are called support vectors."
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
        visualizer_logistic_parameter_controls_ui(session$ns, help_label)
      } else if (selected_algorithm_key() == "knn") {
        visualizer_knn_parameter_controls_ui(
          ns = session$ns,
          help_label = help_label,
          default_knn_distance_metric = default_knn_distance_metric,
          default_knn_voting_method = default_knn_voting_method
        )
      } else if (selected_algorithm_key() == "svm") {
        visualizer_svm_parameter_controls_ui(
          ns = session$ns,
          help_label = help_label,
          default_svm_cost = default_svm_cost,
          default_svm_gamma = default_svm_gamma,
          default_svm_degree = default_svm_degree
        )
      } else {
        visualizer_parameter_placeholder_ui()
      }
    })

    output$run_helper_text <- renderText({
      if (selected_algorithm_key() == "logistic_regression") {
        "Run to update the boundary, metrics, and training views."
      } else if (selected_algorithm_key() == "knn") {
        "Run to update k-NN decision regions and metrics."
      } else if (selected_algorithm_key() == "svm") {
        "Run to update SVM decision regions, margins, support vectors, and metrics."
      } else {
        "This algorithm is coming soon. Choose Logistic Regression to run the classifier."
      }
    })

    observeEvent(input$run_defaults_button, {
      if (selected_algorithm_key() == "logistic_regression") {
        updateSliderInput(session, "logistic_learning_rate", value = default_logistic_learning_rate)
        updateSliderInput(session, "logistic_max_iter", value = default_logistic_max_iter)
        updateSliderInput(session, "decision_threshold", value = default_decision_threshold)
        updateCheckboxInput(session, "logistic_fit_intercept", value = default_logistic_fit_intercept)
      } else if (selected_algorithm_key() == "knn") {
        updateSliderInput(session, "knn_k", value = default_knn_k)
        updateSelectInput(session, "knn_distance_metric", selected = default_knn_distance_metric)
        updateSelectInput(session, "knn_voting_method", selected = default_knn_voting_method)
      } else if (selected_algorithm_key() == "svm") {
        updateSelectInput(session, "svm_kernel", selected = "linear")
        updateSliderInput(session, "svm_cost", value = default_svm_cost)
        updateSliderInput(session, "svm_gamma", value = default_svm_gamma)
        updateSliderInput(session, "svm_degree", value = default_svm_degree)
      }
    }, ignoreInit = TRUE)

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
          knn_distance_metric <- input$knn_distance_metric
          knn_voting_method <- input$knn_voting_method

          if (is.null(knn_k)) {
            knn_k <- default_knn_k
          }
          if (is.null(knn_distance_metric)) {
            knn_distance_metric <- default_knn_distance_metric
          }
          if (is.null(knn_voting_method)) {
            knn_voting_method <- default_knn_voting_method
          }

          list(
            knn_k = knn_k,
            knn_distance_metric = knn_distance_metric,
            knn_voting_method = knn_voting_method
          )
        } else if (selected_algorithm_key() == "svm") {
          svm_kernel <- input$svm_kernel
          svm_cost <- input$svm_cost
          svm_gamma <- input$svm_gamma
          svm_degree <- input$svm_degree

          if (is.null(svm_kernel)) {
            svm_kernel <- "linear"
          }
          if (is.null(svm_cost)) {
            svm_cost <- default_svm_cost
          }
          if (is.null(svm_gamma)) {
            svm_gamma <- default_svm_gamma
          }
          if (is.null(svm_degree)) {
            svm_degree <- default_svm_degree
          }

          list(
            svm_kernel = svm_kernel,
            svm_cost = svm_cost,
            svm_gamma = svm_gamma,
            svm_degree = svm_degree
          )
        } else {
          list()
        }
      }),
      default_algorithm_parameters = reactive({
        if (selected_algorithm_key() == "logistic_regression") {
          list(
            logistic_learning_rate = default_logistic_learning_rate,
            logistic_max_iter = default_logistic_max_iter,
            decision_threshold = default_decision_threshold,
            logistic_fit_intercept = default_logistic_fit_intercept
          )
        } else if (selected_algorithm_key() == "knn") {
          list(
            knn_k = default_knn_k,
            knn_distance_metric = default_knn_distance_metric,
            knn_voting_method = default_knn_voting_method
          )
        } else if (selected_algorithm_key() == "svm") {
          list(
            svm_kernel = "linear",
            svm_cost = default_svm_cost,
            svm_gamma = default_svm_gamma,
            svm_degree = default_svm_degree
          )
        } else {
          list()
        }
      }),
      run_model_clicks = reactive(input$run_classifier_button),
      run_defaults_clicks = reactive(input$run_defaults_button)
    )
  })
}
