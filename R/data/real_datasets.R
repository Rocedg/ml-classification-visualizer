# Real preset dataset helpers for the ML Visualizer app.

# load_local_preset_dataset()
# Purpose:
#   Read a bundled static CSV preset without requiring external dataset packages.
# Inputs:
#   - file_name: CSV file stored in static/
#   - dataset_label: user-facing preset name for clear error messages
#   - points_per_class: maximum examples to keep per class
# Output:
#   A standardized Class A / Class B dataset.
load_local_preset_dataset <- function(file_name, dataset_label, points_per_class = 40) {
  candidate_paths <- c(
    file.path("static", file_name),
    file.path(getwd(), "static", file_name)
  )
  dataset_path <- candidate_paths[file.exists(candidate_paths)][1]

  if (is.na(dataset_path)) {
    stop(paste0("Could not find the local data file for ", dataset_label, ". Expected static/", file_name, "."))
  }

  local_table <- tryCatch(
    read.csv(dataset_path, stringsAsFactors = FALSE),
    error = function(error_object) {
      stop(paste0("Could not read the local data file for ", dataset_label, ": ", error_object$message))
    }
  )

  names(local_table) <- tolower(names(local_table))
  required_columns <- c("x", "y", "class")
  missing_columns <- setdiff(required_columns, names(local_table))

  if (length(missing_columns) > 0) {
    stop(paste0(dataset_label, " must contain the columns: x, y, and class."))
  }

  local_table <- local_table[, required_columns]
  local_table <- local_table[stats::complete.cases(local_table), , drop = FALSE]
  local_table$x <- as.numeric(local_table$x)
  local_table$y <- as.numeric(local_table$y)
  local_table <- local_table[stats::complete.cases(local_table), , drop = FALSE]

  if (nrow(local_table) == 0) {
    stop(paste0(dataset_label, " does not contain any complete rows."))
  }

  class_values <- as.character(local_table$class)
  observed_classes <- unique(class_values)

  if (all(observed_classes %in% c("Class A", "Class B"))) {
    standardized_class_labels <- class_values
  } else if (length(observed_classes) == 2) {
    standardized_class_labels <- ifelse(class_values == observed_classes[1], "Class A", "Class B")
  } else {
    stop(paste0(dataset_label, " must contain exactly two class labels."))
  }

  local_table$class <- factor(standardized_class_labels, levels = c("Class A", "Class B"))

  selected_rows <- unlist(
    lapply(c("Class A", "Class B"), function(class_label) {
      class_rows <- which(local_table$class == class_label)

      if (length(class_rows) == 0) {
        stop(paste0(dataset_label, " must contain both Class A and Class B."))
      }

      sample(class_rows, size = min(points_per_class, length(class_rows)))
    }),
    use.names = FALSE
  )

  selected_data <- local_table[selected_rows, , drop = FALSE]
  create_indexed_dataset(selected_data$x, selected_data$y, selected_data$class)
}
