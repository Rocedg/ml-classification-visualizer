# modules/model_theory_panel_module.R
# Purpose:
#   Display a beginner-friendly explanation of the currently selected model.
#   This tab combines conceptual explanations with the latest metric values.
#
# Functions:
#   - model_theory_panel_module_ui(): Build the Model Theory tab layout.
#   - model_theory_panel_module_server(): Render algorithm-specific content.
#
# Inputs / Outputs:
#   Inputs:
#     - Reactive selected algorithm key
#     - Reactive trained model bundle
#   Outputs:
#     - Styled explanatory content for the selected model


mod_visualizer_model_explanation_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "theory-panel-layout",
    uiOutput(ns("model_theory_content_ui"))
  )
}


mod_visualizer_model_explanation_server <- function(id, selected_algorithm_key, trained_model_bundle) {
  moduleServer(id, function(input, output, session) {

    # Centralised per-model configuration
    model_theory_content <- list(

      logistic_regression = list(
        title = "Logistic Regression",

        explanation = tagList(

          # ── 1. What problem does it solve? ──────────────────────────────
          tags$h4("What problem does it solve?"),
          tags$p(
            "Logistic Regression is a ", tags$b("binary classification"), " algorithm.",
            " Given a set of input features (e.g. age, weight, blood pressure) it predicts",
            " the probability that an observation belongs to one of two classes (0 or 1).",
            " Unlike linear regression, the output is always a number between 0 and 1."
          ),

          # ── 2. Step 1 — linear combination ──────────────────────────────
          tags$h4("Step 1 — Linear combination"),
          tags$p(
            "The model starts by computing a weighted sum of the input features,",
            " exactly like a straight line:"
          ),
          tags$div(
            class = "math-box",
            withMathJax("$$z = w_1 x_1 + w_2 x_2 + \\cdots + w_n x_n + b = \\mathbf{w}^\\top \\mathbf{x} + b$$")
          ),
          tags$ul(
            tags$li(withMathJax("\\(w_j\\) — weights: what the model learns during training.")),
            tags$li(withMathJax("\\(x_j\\) — input features: the data you provide.")),
            tags$li(withMathJax("\\(b\\) — bias term (intercept).")),
            tags$li(withMathJax("\\(z\\) — raw score, can be any real number \\((-\\infty, +\\infty)\\)."))
          ),

          # ── 3. Step 2 — sigmoid function ────────────────────────────────
          tags$h4("Step 2 — Sigmoid function"),
          tags$p(
            "Because ", tags$em("z"), " can be any number, we cannot use it directly as a probability.",
            " We pass it through the ", tags$b("sigmoid (logistic) function"),
            ", which squishes any value into the interval (0, 1):"
          ),
          tags$div(
            class = "math-box",
            withMathJax("$$\\hat{y} = \\sigma(z) = \\frac{1}{1 + e^{-z}}$$")
          ),
          tags$ul(
            tags$li(withMathJax("When \\(z = 0 \\Rightarrow \\hat{y} = 0.5\\) — the model is completely uncertain.")),
            tags$li(withMathJax("When \\(z \\to +\\infty \\Rightarrow \\hat{y} \\to 1\\) — confident it is class 1.")),
            tags$li(withMathJax("When \\(z \\to -\\infty \\Rightarrow \\hat{y} \\to 0\\) — confident it is class 0."))
          ),

          # ── 4. Cost function ─────────────────────────────────────────────
          tags$h4("Cost function — Log-loss (Binary Cross-Entropy)"),
          tags$p(
            "To measure how wrong the model's predictions are, logistic regression uses",
            " the ", tags$b("log-loss"), " function instead of Mean Squared Error.",
            " MSE applied to a sigmoid produces a non-convex surface with many local minima,",
            " making optimisation unreliable.",
            " Log-loss is guaranteed to be ", tags$b("convex"), " — it has exactly one global minimum."
          ),
          tags$p("For a single training example:"),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$L(\\hat{y}, y) = -\\left[ y \\cdot \\log(\\hat{y}) + (1 - y) \\cdot \\log(1 - \\hat{y}) \\right]$$"
            )
          ),
          tags$p("Averaged over the full dataset of N examples:"),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$J(\\mathbf{w}, b) = -\\frac{1}{N} \\sum_{i=1}^{N} \\left[ y_i \\cdot \\log(\\hat{y}_i) + (1 - y_i) \\cdot \\log(1 - \\hat{y}_i) \\right]$$"
            )
          ),
          tags$ul(
            tags$li(withMathJax("If \\(y = 1\\) and \\(\\hat{y} \\approx 1\\): cost \\(\\approx 0\\) ✓")),
            tags$li(withMathJax("If \\(y = 1\\) and \\(\\hat{y} \\approx 0\\): cost \\(\\to \\infty\\) ✗  — large penalty for a confident wrong answer.")),
            tags$li(withMathJax("If \\(y = 0\\) and \\(\\hat{y} \\approx 0\\): cost \\(\\approx 0\\) ✓")),
            tags$li(withMathJax("If \\(y = 0\\) and \\(\\hat{y} \\approx 1\\): cost \\(\\to \\infty\\) ✗"))
          ),

          # ── 5. Gradient descent ──────────────────────────────────────────
          tags$h4("Gradient Descent — how the model learns"),
          tags$p(
            "To minimise ", tags$em("J"), ", the model uses ",
            tags$b("gradient descent"), ": an iterative algorithm that repeatedly",
            " nudges each weight in the direction that reduces the cost.",
            " Think of it as a ball rolling downhill on the cost surface until it reaches the lowest point."
          ),
          tags$p(
            "The gradient of ", tags$em("J"), " with respect to each weight is computed",
            " via the ", tags$b("chain rule"), " (three nested derivatives):"
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\frac{\\partial J}{\\partial w_j} =
                \\underbrace{\\frac{\\partial J}{\\partial \\hat{y}}}_{\\text{log-loss deriv.}} \\cdot
                \\underbrace{\\frac{d\\hat{y}}{dz}}_{\\text{sigmoid deriv.}} \\cdot
                \\underbrace{\\frac{\\partial z}{\\partial w_j}}_{= x_j}$$"
            )
          ),
          tags$p(
            "After the chain rule, the ", tags$em("sigmoid"), " and ", tags$em("log-loss"),
            " terms cancel perfectly, leaving a beautifully simple result:"
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\frac{\\partial J}{\\partial w_j} = \\frac{1}{N} \\sum_{i=1}^{N} (\\hat{y}_i - y_i) \\cdot x_{ij}
              \\qquad
              \\frac{\\partial J}{\\partial b} = \\frac{1}{N} \\sum_{i=1}^{N} (\\hat{y}_i - y_i)$$"
            )
          ),
          tags$p("The gradient descent update rule (applied simultaneously to all parameters):"),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$w_j := w_j - \\eta \\cdot \\frac{\\partial J}{\\partial w_j}
              \\qquad
              b := b - \\eta \\cdot \\frac{\\partial J}{\\partial b}$$"
            )
          ),
          tags$ul(
            tags$li(withMathJax("\\(\\eta\\) (eta) — the ", tags$b("learning rate"), ": controls step size. Too large → overshoots. Too small → very slow.")),
            tags$li("This loop repeats until the cost stops decreasing — called ", tags$b("convergence"), ".")
          ),

          # ── 6. Regularisation ────────────────────────────────────────────
          tags$h4("Regularisation — preventing overfitting"),
          tags$p(
            "If the weights grow very large, the model memorises training data noise instead of",
            " learning general patterns. This is called ", tags$b("overfitting"), ".",
            " Regularisation adds a penalty term to the cost function that discourages large weights:"
          ),
          tags$p(tags$b("L2 regularisation (Ridge):")),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$J_{\\text{reg}}(\\mathbf{w}) = J(\\mathbf{w}) + \\lambda \\cdot \\frac{1}{2} \\sum_j w_j^2$$"
            )
          ),
          tags$p(tags$b("L1 regularisation (Lasso):")),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$J_{\\text{reg}}(\\mathbf{w}) = J(\\mathbf{w}) + \\lambda \\cdot \\sum_j |w_j|$$"
            )
          ),
          tags$ul(
            tags$li(withMathJax("\\(\\lambda\\) — the ", tags$b("regularisation strength"), ": larger \\(\\lambda\\) forces weights closer to zero.")),
            tags$li("L2 shrinks all weights smoothly — all features stay in the model."),
            tags$li("L1 can drive some weights to exactly zero — effectively removing irrelevant features."),
            tags$li(withMathJax("\\(\\lambda = 0\\) means no regularisation (risk of overfitting); very large \\(\\lambda\\) risks underfitting."))
          ),

          # ── 7. Decision boundary ─────────────────────────────────────────
          tags$h4("Decision boundary"),
          tags$p(
            "The model assigns class 1 when ", withMathJax("\\(\\hat{y} \\geq \\tau\\)"),
            " and class 0 otherwise, where ", withMathJax("\\(\\tau\\)"), " is the decision threshold (default 0.5).",
            " Because the sigmoid is a monotonic function of ", tags$em("z"),
            ", this boundary corresponds to the hyperplane where ", withMathJax("\\(z = 0\\)"), ":"
          ),
          tags$div(
            class = "math-box",
            withMathJax("$$w_1 x_1 + w_2 x_2 + \\cdots + w_n x_n + b = 0$$")
          ),
          tags$ul(
            tags$li("Lower threshold → more points classified as Class 1 (higher recall, lower precision)."),
            tags$li("Higher threshold → stricter classification (higher precision, lower recall).")
          )
        ),

        strengths = tagList(
          tags$li("Easy to interpret — each weight directly reflects a feature's influence on the log-odds."),
          tags$li("Fast to train and computationally efficient baseline model."),
          tags$li("Produces well-calibrated probabilities, not just class labels."),
          tags$li("Works well when classes are roughly linearly separable."),
          tags$li("Regularisation (L1/L2) gives explicit control over model complexity.")
        ),

        parameter_note = tagList(
          tags$p(tags$b("Decision threshold"), withMathJax("\\((\\tau)\\)"), ":"),
          tags$p("Shifts the decision boundary between the two classes."),
          tags$ul(
            tags$li(withMathJax("Lower \\(\\tau\\) → more points classified as Class 1 (useful when false negatives are costly).")),
            tags$li(withMathJax("Higher \\(\\tau\\) → stricter classification (useful when false positives are costly)."))
          ),
          tags$p(tags$b("Regularisation strength"), withMathJax("\\((\\lambda\\) or \\(C = 1/\\lambda)\\)"), ":"),
          tags$ul(
            tags$li(withMathJax("Large \\(\\lambda\\) (small \\(C\\)) → strong regularisation, simpler model.")),
            tags$li(withMathJax("Small \\(\\lambda\\) (large \\(C\\)) → weak regularisation, model fits data more closely."))
          )
        )
      ),

      # ── SVM ───────────────────────────────────────────────────────────────
      svm = list(
        title = "Support Vector Machine",
        explanation = "SVM searches for a separating boundary that maximises the margin between classes. With a radial kernel, the boundary can bend around more complex shapes.",
        strengths = c(
          "Can learn nonlinear boundaries",
          "Often performs well on complex shapes",
          "Margin-based decision rule"
        ),
        parameter_note = "Main parameters in this app: kernel, cost, and gamma."
      ),

      # ── KNN ───────────────────────────────────────────────────────────────
      knn = list(
        title = "k-Nearest Neighbors",
        explanation = "k-NN does not fit a single equation. Instead, it looks at nearby labelled points and uses their votes to decide the class for each location.",
        strengths = c(
          "Very intuitive",
          "Responds to local structure",
          "Useful for comparing neighbourhood effects"
        ),
        parameter_note = "Main parameter in this app: number of neighbors (k)."
      )
    )

    output$model_theory_content_ui <- renderUI({

      algorithm_key  <- selected_algorithm_key()
      model_results  <- tryCatch(trained_model_bundle(), error = function(e) NULL)

      model_info <- model_theory_content[[algorithm_key]]

      if (is.null(model_info)) {
        return(tags$p("No theory available for this model."))
      }

      metrics_block <- if (is.null(model_results)) {
        div(
          class = "app-card theory-summary-card",
          tags$h4("Current Run Summary"),
          tags$p("Run the classifier from the sidebar to populate this summary with live metrics.")
        )
      } else {
        div(
          class = "app-card theory-summary-card",
          tags$h4("Current Run Summary"),
          tags$p(paste("Algorithm:",  model_results$algorithm_label)),
          tags$p(paste("Accuracy:",   model_results$metrics$accuracy)),
          tags$p(paste("Precision:",  model_results$metrics$precision)),
          tags$p(paste("Recall:",     model_results$metrics$recall)),
          tags$p(paste("F1 Score:",   model_results$metrics$f1_score))
        )
      }

      div(
        class = "theory-panel-grid",

        div(
          class = "theory-main",
          div(
            class = "app-card theory-detail-card",
            tags$h3(model_info$title),
            model_info$explanation,

            tags$h4("Why it helps in this app"),
            tags$ul(model_info$strengths),

            tags$h4("Parameter focus"),
            model_info$parameter_note
          )
        ),

        div(
          class = "theory-side",
          metrics_block
        )
      )
    })
  })
}
