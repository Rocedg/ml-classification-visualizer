# Iteration navigation helper functions for the ML Visualizer app.

# model_supports_logistic_playback()
# Purpose:
#   Tell the UI whether a model result contains logistic iteration history.
model_supports_logistic_playback <- function(model_results) {
  !is.null(model_results) && identical(model_results$algorithm_key, "logistic_regression")
}


# get_logistic_iteration_history()
# Purpose:
#   Extract the saved training states used by slider and step-button playback.
get_logistic_iteration_history <- function(model_results) {
  if (!model_supports_logistic_playback(model_results)) {
    return(NULL)
  }

  model_results$iterations
}


# get_iteration_count()
# Purpose:
#   Return how many saved states are available for playback.
get_iteration_count <- function(iteration_history) {
  if (is.null(iteration_history)) {
    return(0)
  }

  length(iteration_history)
}


# bound_iteration_index()
# Purpose:
#   Clamp a requested iteration so it always points to an existing saved state.
bound_iteration_index <- function(iteration_index, iteration_count) {
  if (is.null(iteration_index) || is.null(iteration_count) || iteration_count < 1) {
    return(1)
  }

  min(max(iteration_index, 1), iteration_count)
}


# get_active_iteration_results()
# Purpose:
#   Select the model view that should drive the heatmap, metrics, and diagnostics.
# Output:
#   NULL before training, full model results for non-playback models, or one
#   saved logistic iteration for the current playback position.
get_active_iteration_results <- function(model_results, iteration_history, current_iteration) {
  if (is.null(model_results)) {
    return(NULL)
  }

  if (!model_supports_logistic_playback(model_results)) {
    return(model_results)
  }

  if (is.null(iteration_history) || length(iteration_history) == 0) {
    return(model_results)
  }

  bounded_iteration <- bound_iteration_index(current_iteration, length(iteration_history))
  iteration_history[[bounded_iteration]]
}


# format_iteration_status_text()
# Purpose:
#   Build the small status label that tells users which iteration is displayed.
format_iteration_status_text <- function(model_results, current_iteration, total_iterations) {
  if (!model_supports_logistic_playback(model_results)) {
    return("Iteration navigation ready after Logistic Regression")
  }

  iteration_history <- get_logistic_iteration_history(model_results)
  if (is.null(iteration_history) || length(iteration_history) == 0) {
    return("Iteration navigation ready after Logistic Regression")
  }

  bounded_iteration <- bound_iteration_index(current_iteration, length(iteration_history))
  active_iteration <- iteration_history[[bounded_iteration]]
  final_iteration <- iteration_history[[length(iteration_history)]]

  paste("Iteration:", active_iteration$iteration_index, "/", final_iteration$iteration_index)
}


# format_iteration_navigation_status_text()
# Purpose:
#   Build helper text for the manual iteration controls.
format_iteration_navigation_status_text <- function(model_results) {
  if (!model_supports_logistic_playback(model_results)) {
    return("Manual navigation ready after Logistic Regression")
  }

  "Manual navigation"
}


# format_playback_helper_text()
# Purpose:
#   Explain the currently selected training state in compact UI text.
format_playback_helper_text <- function(model_results, active_iteration) {
  if (is.null(model_results)) {
    return("Run Logistic Regression to inspect the training iterations.")
  }

  if (!model_supports_logistic_playback(model_results)) {
    return("Iteration navigation is currently available only for Logistic Regression.")
  }

  if (is.null(active_iteration)) {
    return("Iteration details are not available for this model run.")
  }

  paste(
    "Loss:",
    active_iteration$loss_value,
    "| Use the slider or step buttons to move through how the heatmap changes over time."
  )
}
