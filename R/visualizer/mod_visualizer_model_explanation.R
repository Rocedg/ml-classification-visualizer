# modules/model_theory_panel_module.R
# Purpose:
#   Display a beginner-friendly explanation of the currently selected model.
#   This tab combines conceptual explanations with the latest metric values.
#
# Functions:
#   - mod_visualizer_model_explanation_ui(): Build the Model Theory tab layout.
#   - mod_visualizer_model_explanation_server(): Render algorithm-specific content.
#
# Inputs / Outputs:
#   Inputs:
#     - Reactive selected algorithm key
#   Outputs:
#     - Styled explanatory content for the selected model


mod_visualizer_model_explanation_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "theory-panel-layout",
    uiOutput(ns("model_theory_content_ui"))
  )
}


mod_visualizer_model_explanation_server <- function(id, selected_algorithm_key) {
  moduleServer(id, function(input, output, session) {
    
    # Centralised per-model configuration
    model_theory_content <- list(
      
      # ── LOGISTIC REGRESSION ───────────────────────────────────────────────
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
        
        explanation = tagList(
          
          # ── 1. What problem does it solve? ──────────────────────────────
          tags$h4("What problem does it solve?"),
          tags$p(
            "k-Nearest Neighbors (k-NN) is a ", tags$b("non-parametric, instance-based"), " classification algorithm.",
            " Unlike logistic regression, it does not fit an equation to the data during training.",
            " Instead, it memorises the entire training set and, at prediction time,",
            " looks up the ", tags$b("K most similar points"), " to decide the class of a new observation.",
            " Because it defers all computation to prediction time, it is also called a ",
            tags$b("lazy learner"), "."
          ),
          tags$p(
            "Intuition: if you move to a new city and want to know whether a neighbourhood is",
            " quiet or lively, you simply look at your closest neighbours and go with the majority."
          ),
          
          # ── 2. What is K? ────────────────────────────────────────────────
          tags$h4("What is K?"),
          tags$p(
            tags$b("K"), " is the single hyperparameter of the algorithm.",
            " It tells the model how many neighbouring training points to consult",
            " before making a decision."
          ),
          tags$ul(
            tags$li(withMathJax("\\(K = 1\\) — the new point inherits the class of its single nearest neighbour.")),
            tags$li(withMathJax("\\(K = 3\\) — the three closest points each cast one vote; majority wins.")),
            tags$li(withMathJax("\\(K = N\\) (all points) — every point votes; the model always predicts the most frequent class in the dataset (useless)."))
          ),
          tags$p(
            "In practice K is chosen by cross-validation. Using an ",
            tags$b("odd K"), " avoids ties when there are two classes."
          ),
          
          # ── 3. Step 1 — distance metric ──────────────────────────────────
          tags$h4("Step 1 — Measure distance"),
          tags$p(
            "To rank training points by similarity, k-NN needs a ", tags$b("distance metric"),
            ". The three most common are:"
          ),
          
          tags$p(tags$b("Euclidean distance (L2) — straight-line distance in feature space:")),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$d_{\\text{Euclidean}}(\\mathbf{x}, \\mathbf{x}') =
              \\sqrt{\\sum_{j=1}^{p} (x_j - x'_j)^2}$$"
            )
          ),
          
          tags$p(tags$b("Manhattan distance (L1) — sum of absolute differences, like navigating a grid:")),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$d_{\\text{Manhattan}}(\\mathbf{x}, \\mathbf{x}') =
              \\sum_{j=1}^{p} |x_j - x'_j|$$"
            )
          ),
          
          tags$p(
            tags$b("Minkowski distance — a unified family that contains both:"),
            " setting ", withMathJax("\\(p = 2\\)"),
            " gives Euclidean; setting ", withMathJax("\\(p = 1\\)"), " gives Manhattan."
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$d_{\\text{Minkowski}}(\\mathbf{x}, \\mathbf{x}') =
              \\left( \\sum_{j=1}^{p} |x_j - x'_j|^{\\,q} \\right)^{1/q}$$"
            )
          ),
          tags$ul(
            tags$li(withMathJax("\\(\\mathbf{x}\\) — the new (query) point to classify.")),
            tags$li(withMathJax("\\(\\mathbf{x}'\\) — a training point.")),
            tags$li(withMathJax("\\(p\\) — number of features (dimensions)."))
          ),
          tags$p(
            tags$b("Important:"), " because distance is sensitive to scale,",
            " features should be standardised (zero mean, unit variance) before applying k-NN.",
            " A feature measured in kilometres would otherwise dominate one measured in metres."
          ),
          
          # ── 4. Step 2 — find K nearest neighbours ────────────────────────
          tags$h4("Step 2 — Find the K nearest neighbours"),
          tags$p(
            "Compute the distance from the new point ", withMathJax("\\(\\mathbf{x}_{\\text{new}}\\)"),
            " to every training point ", withMathJax("\\(\\mathbf{x}_i\\)"),
            ", then sort all N distances in ascending order and take the first K:"
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\mathcal{N}_K(\\mathbf{x}_{\\text{new}}) =
              \\text{the } K \\text{ training points with smallest }
              d(\\mathbf{x}_{\\text{new}},\\, \\mathbf{x}_i)$$"
            )
          ),
          tags$p(
            "Visually, this corresponds to drawing a circle (in 2D) or a sphere (in 3D)",
            " centred on the new point and expanding it until it touches exactly K training points."
          ),
          
          # ── 5. Step 3 — majority vote ─────────────────────────────────────
          tags$h4("Step 3 — Majority vote (classification)"),
          tags$p(
            "Each of the K neighbours casts one vote for its own class label.",
            " The new point is assigned to the class that receives the most votes:"
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\hat{y} = \\underset{c}{\\operatorname{arg\\,max}}
              \\sum_{i \\in \\mathcal{N}_K(\\mathbf{x}_{\\text{new}})} \\mathbf{1}\\{y_i = c\\}$$"
            )
          ),
          tags$ul(
            tags$li(withMathJax("\\(c\\) — a candidate class label.")),
            tags$li(withMathJax("\\(\\mathbf{1}\\{y_i = c\\}\\) — indicator function: 1 if neighbour \\(i\\) belongs to class \\(c\\), 0 otherwise.")),
            tags$li(withMathJax("\\(\\hat{y}\\) — the predicted class: the one with the highest vote count."))
          ),
          tags$p(
            "Example with K = 5: neighbours have labels [A, A, B, A, B].",
            " Class A gets 3 votes, Class B gets 2. Prediction: ", tags$b("A"), "."
          ),
          
          # ── 6. Worked numeric example ─────────────────────────────────────
          tags$h4("Worked numeric example"),
          tags$p(
            "Suppose we have four training points in 2D (features: height, weight)",
            " and we want to classify a new point ", withMathJax("\\(\\mathbf{x}_{\\text{new}} = (4, 5)\\)"),
            " using K = 3."
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\begin{array}{c|cc|c|c}
              \\text{Point} & x_1 & x_2 & \\text{Class} & d_{\\text{Euclidean}} \\\\
              \\hline
              A_1 & 1 & 2 & A & \\sqrt{(4-1)^2+(5-2)^2} = \\sqrt{18} \\approx 4.24 \\\\
              A_2 & 2 & 3 & A & \\sqrt{(4-2)^2+(5-3)^2} = \\sqrt{8}  \\approx 2.83 \\\\
              A_3 & 3 & 4 & A & \\sqrt{(4-3)^2+(5-4)^2} = \\sqrt{2}  \\approx 1.41 \\\\
              B_1 & 6 & 7 & B & \\sqrt{(4-6)^2+(5-7)^2} = \\sqrt{8}  \\approx 2.83 \\\\
              B_2 & 7 & 8 & B & \\sqrt{(4-7)^2+(5-8)^2} = \\sqrt{18} \\approx 4.24
              \\end{array}$$"
            )
          ),
          tags$p(
            "Sorted by distance: ", withMathJax("\\(A_3 (1.41),\\; A_2 (2.83),\\; B_1 (2.83)\\)"),
            ". The three nearest neighbours are ", tags$b("A, A, B"),
            ". Votes: A = 2, B = 1. Prediction: ", tags$b("Class A"), "."
          ),
          
          # ── 7. Effect of K — bias-variance tradeoff ───────────────────────
          tags$h4("Effect of K — bias-variance tradeoff"),
          tags$p(
            "The choice of K directly controls the model's ", tags$b("bias-variance tradeoff"), ":"
          ),
          tags$ul(
            tags$li(
              tags$b("Small K (e.g. K = 1):"),
              " the decision boundary is very jagged and closely follows every training point.",
              " Low bias, high variance — prone to ", tags$b("overfitting"), ".",
              " A single outlier can flip the prediction."
            ),
            tags$li(
              tags$b("Large K (e.g. K = N):"),
              " the boundary becomes very smooth — in the extreme, always predicts the majority class.",
              " High bias, low variance — prone to ", tags$b("underfitting"), ".",
              " Local patterns are lost."
            ),
            tags$li(
              tags$b("Optimal K:"),
              " found by ", tags$b("cross-validation"),
              " — split the data into folds, train on some, evaluate on the rest,",
              " and pick the K that minimises validation error."
            )
          ),
          tags$p(
            "A useful rule of thumb: start with ", withMathJax("\\(K = \\sqrt{N}\\)"),
            " (square root of the number of training samples) and tune from there."
          ),
          
          # ── 8. Weighted voting ────────────────────────────────────────────
          tags$h4("Weighted voting — an extension"),
          tags$p(
            "Standard k-NN gives every neighbour equal weight.",
            " A common improvement is to weight each vote by the ", tags$b("inverse of its distance"),
            ", so closer neighbours have more influence:"
          ),
          tags$div(
            class = "math-box",
            withMathJax(
              "$$\\hat{y} = \\underset{c}{\\operatorname{arg\\,max}}
              \\sum_{i \\in \\mathcal{N}_K} \\frac{1}{d(\\mathbf{x}_{\\text{new}}, \\mathbf{x}_i)} \\cdot \\mathbf{1}\\{y_i = c\\}$$"
            )
          ),
          tags$p(
            "If a neighbour is at distance 0 (exact match), it is assigned infinite weight",
            " and its class is returned directly."
          ),
          
        
        strengths = tagList(
          tags$li("Extremely intuitive — the algorithm mirrors human reasoning about similarity."),
          tags$li(
            tags$b("No training phase"), " — the model simply memorises the dataset,",
            " making it trivial to add new training points."
          ),
          tags$li("Non-parametric — makes no assumptions about the shape of the decision boundary."),
          tags$li("Naturally handles multi-class problems without any modification."),
          tags$li("Works well for small datasets with complex, irregular boundaries."),
          tags$li("Responds sensitively to local structure in the data.")
        ),
        
        parameter_note = tagList(
          tags$p(tags$b("Number of neighbours"), withMathJax("\\((K)\\)"), ":"),
          tags$p(
            "The single most important parameter.",
            " Controls the smoothness of the decision boundary."
          ),
          tags$ul(
            tags$li(withMathJax("Small \\(K\\) (e.g. 1–3) → complex, jagged boundary — risk of overfitting.")),
            tags$li(withMathJax("Large \\(K\\) → smooth boundary — risk of underfitting and ignoring local patterns.")),
            tags$li(
              "Use ", tags$b("odd K"), " with two-class problems to avoid ties.",
              " Cross-validation is the principled way to choose K."
            )
          ),
          tags$p(tags$b("Distance metric"), ":"),
          tags$ul(
            tags$li("Euclidean (L2) is the default and works well for continuous features on a similar scale."),
            tags$li("Manhattan (L1) is more robust to outliers in individual dimensions."),
            tags$li(
              tags$b("Always standardise features"), " before fitting k-NN —",
              " a feature with a large numeric range will otherwise dominate the distance computation."
            )
          )
        )
      )
    )
    
    output$model_theory_content_ui <- renderUI({
      
      algorithm_key <- selected_algorithm_key()
      model_info    <- model_theory_content[[algorithm_key]]
      
      if (is.null(model_info)) {
        return(tags$p("No theory available for this model."))
      }
      
      div(
        class = "app-card theory-detail-card",
        tags$h3(model_info$title),
        model_info$explanation,
        
        tags$h4("Why it helps in this app"),
        tags$ul(model_info$strengths),
        
        tags$h4("Parameter focus"),
        model_info$parameter_note
      )
    })
  })
}
