# R/visualizer/mod_visualizer_parameters.R
# Purpose:
#   Build algorithm-specific parameter controls for the visualizer sidebar.

visualizer_logistic_parameter_controls_ui <- function(ns, help_label) {
  tagList(
    sliderInput(
      inputId = ns("logistic_learning_rate"),
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
      inputId = ns("logistic_max_iter"),
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
      inputId = ns("decision_threshold"),
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
      inputId = ns("logistic_fit_intercept"),
      label = help_label(
        "Fit intercept",
        "Allows the decision boundary to shift by learning a bias term."
      ),
      value = TRUE
    )
  )
}


visualizer_knn_parameter_controls_ui <- function(ns,
                                                 help_label,
                                                 default_knn_distance_metric = "euclidean",
                                                 default_knn_voting_method = "uniform") {
  tagList(
    sliderInput(
      inputId = ns("knn_k"),
      label = help_label(
        "k neighbors",
        "Number of nearest training points used to vote for the predicted class."
      ),
      min = 1,
      max = 25,
      value = 5,
      step = 1
    ),
    selectInput(
      inputId = ns("knn_distance_metric"),
      label = help_label(
        "Distance metric",
        "Euclidean is straight-line distance. Manhattan adds horizontal and vertical distance."
      ),
      choices = c(
        "Euclidean" = "euclidean",
        "Manhattan" = "manhattan"
      ),
      selected = default_knn_distance_metric
    ),
    selectInput(
      inputId = ns("knn_voting_method"),
      label = help_label(
        "Voting method",
        "Uniform gives each neighbor one vote. Distance-weighted gives closer neighbors more influence."
      ),
      choices = c(
        "Uniform" = "uniform",
        "Distance-weighted" = "distance_weighted"
      ),
      selected = default_knn_voting_method
    )
  )
}


visualizer_svm_parameter_controls_ui <- function(ns,
                                                 help_label,
                                                 default_svm_cost = 1,
                                                 default_svm_gamma = 0.5,
                                                 default_svm_degree = 3) {
  tagList(
    selectInput(
      inputId = ns("svm_kernel"),
      label = help_label(
        "Kernel",
        "Controls how the SVM transforms the feature space before finding a separating boundary."
      ),
      choices = c(
        "Linear" = "linear",
        "RBF" = "radial",
        "Polynomial" = "polynomial"
      ),
      selected = "linear"
    ),
    sliderInput(
      inputId = ns("svm_cost"),
      label = help_label(
        "C / Cost",
        "Controls how strongly margin violations are penalized. Lower C allows a wider margin with more mistakes; higher C fits the training data more tightly."
      ),
      min = 0.01,
      max = 100,
      value = default_svm_cost,
      step = 0.01
    ),
    conditionalPanel(
      condition = paste0(
        "input['", ns("svm_kernel"), "'] == 'radial' || ",
        "input['", ns("svm_kernel"), "'] == 'polynomial'"
      ),
      sliderInput(
        inputId = ns("svm_gamma"),
        label = help_label(
          "Gamma",
          "Controls how much influence each training point has. Higher gamma creates more local, flexible boundaries."
        ),
        min = 0.01,
        max = 10,
        value = default_svm_gamma,
        step = 0.01
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("svm_kernel"), "'] == 'polynomial'"),
      sliderInput(
        inputId = ns("svm_degree"),
        label = help_label(
          "Degree",
          "Controls the complexity of the polynomial boundary. Higher degree can create more curved boundaries."
        ),
        min = 2,
        max = 5,
        value = default_svm_degree,
        step = 1
      )
    )
  )
}


visualizer_parameter_placeholder_ui <- function() {
  tagList(
    div(
      class = "algorithm-placeholder-card",
      tags$span("Coming soon"),
      tags$p("Parameters for this algorithm will appear here when it is enabled.")
    )
  )
}
