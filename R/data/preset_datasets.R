# Preset dataset routing helpers for the ML Visualizer app.

available_preset_dataset_names <- function() {
  c(
    synthetic_preset_dataset_names(),
    "Titanic passengers",
    "Diabetes health data"
  )
}


# generate_preset_dataset()
# Purpose:
#   Create reproducible example datasets for the visualizer sidebar.
# Inputs:
#   - dataset_name: selected preset name from the UI
#   - points_per_class: maximum number of examples to sample per class
# Output:
#   A standardized Class A / Class B dataset created by create_indexed_dataset().
generate_preset_dataset <- function(dataset_name, points_per_class = 40) {
  seed_lookup <- c(
    "Gaussian clusters" = 101,
    "Linearly separable" = 202,
    "Overlapping classes" = 303,
    "Moons" = 404,
    "Circles" = 505,
    "XOR pattern" = 506,
    "Polynomial curve" = 507,
    "Noisy nonlinear blobs" = 508,
    "Titanic passengers" = 606,
    "Diabetes health data" = 707
  )

  if (!dataset_name %in% names(seed_lookup)) {
    stop("Unknown preset dataset selected.")
  }

  set.seed(seed_lookup[[dataset_name]])

  if (is_synthetic_preset_dataset(dataset_name)) {
    return(generate_synthetic_preset_dataset(
      dataset_name = dataset_name,
      points_per_class = points_per_class
    ))
  }

  if (dataset_name == "Titanic passengers") {
    return(load_local_preset_dataset(
      file_name = "titanic_passengers.csv",
      dataset_label = dataset_name,
      points_per_class = points_per_class
    ))
  }

  if (dataset_name == "Diabetes health data") {
    return(load_local_preset_dataset(
      file_name = "diabetes_health_data.csv",
      dataset_label = dataset_name,
      points_per_class = points_per_class
    ))
  }

  stop("Unknown preset dataset selected.")
}
