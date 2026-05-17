# Uploaded dataset helpers for the ML Visualizer app.

# convert_uploaded_csv_to_dataset()
# Purpose:
#   Validate and standardize a user-uploaded CSV.
# Inputs:
#   - uploaded_table: data frame read from a CSV file
# Output:
#   A clean two-class dataset with required x, y, and class columns.
convert_uploaded_csv_to_dataset <- function(uploaded_table) {
  cleaned_names <- tolower(names(uploaded_table))
  names(uploaded_table) <- cleaned_names

  required_columns <- c("x", "y", "class")
  missing_columns <- setdiff(required_columns, names(uploaded_table))

  if (length(missing_columns) > 0) {
    stop("Uploaded CSV must contain the columns: x, y, and class.")
  }

  cleaned_table <- uploaded_table[, required_columns]
  cleaned_table <- cleaned_table[stats::complete.cases(cleaned_table), , drop = FALSE]

  cleaned_table$x <- as.numeric(cleaned_table$x)
  cleaned_table$y <- as.numeric(cleaned_table$y)

  if (any(is.na(cleaned_table$x)) || any(is.na(cleaned_table$y))) {
    stop("Columns x and y must contain numeric values.")
  }

  unique_classes <- unique(as.character(cleaned_table$class))

  if (length(unique_classes) != 2) {
    stop("Uploaded CSV must contain exactly two distinct class labels.")
  }

  # Uploaded labels can use any two names. The app standardizes the first
  # observed class to Class A and the second observed class to Class B.
  standardized_class_labels <- ifelse(
    as.character(cleaned_table$class) == unique_classes[1],
    "Class A",
    "Class B"
  )

  create_indexed_dataset(cleaned_table$x, cleaned_table$y, standardized_class_labels)
}
