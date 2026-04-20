# Playback helper functions for the ML Visualizer app.

model_supports_logistic_playback <- function(model_results) {
  !is.null(model_results) && identical(model_results$algorithm_key, "logistic_regression")
}


get_logistic_iteration_history <- function(model_results) {
  if (!model_supports_logistic_playback(model_results)) {
    return(NULL)
  }

  model_results$iterations
}


get_iteration_count <- function(iteration_history) {
  if (is.null(iteration_history)) {
    return(0)
  }

  length(iteration_history)
}


bound_iteration_index <- function(iteration_index, iteration_count) {
  min(max(iteration_index, 1), iteration_count)
}


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


format_iteration_status_text <- function(model_results, current_iteration, total_iterations) {
  if (!model_supports_logistic_playback(model_results)) {
    return("Iteration playback ready after Logistic Regression")
  }

  paste("Iteration:", current_iteration, "/", total_iterations)
}


format_playback_status_text <- function(model_results, playback_is_running) {
  if (!model_supports_logistic_playback(model_results)) {
    return("Playback inactive")
  }

  if (playback_is_running) {
    "Playback running"
  } else {
    "Playback paused"
  }
}


format_playback_helper_text <- function(model_results, active_iteration) {
  if (is.null(model_results)) {
    return("Run Logistic Regression to unlock the step-by-step training replay controls.")
  }

  if (!model_supports_logistic_playback(model_results)) {
    return("Iteration playback is currently available only for Logistic Regression.")
  }

  paste(
    "Loss:",
    active_iteration$loss_value,
    "| Use the slider or buttons to move through how the boundary changes over time."
  )
}
