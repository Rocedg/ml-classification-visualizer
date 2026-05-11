# Model training helper functions for the ML Visualizer app.
#
# This compatibility file keeps algorithm-agnostic training helpers that have
# not yet moved to a more specific location. Model backends now live in
# R/models/, and the training dispatcher lives in R/model_dispatch_helpers.R.
# create_train_test_split()
# Purpose:
#   Mark the current run data with a reproducible 70/30 train/test split.
# Inputs:
#   - classification_data: current Class A / Class B points
#   - train_fraction: share of points used to fit the model
#   - seed: fixed seed so the same run data gets the same split
# Output:
#   The same data with a split column containing "train" or "test".
create_train_test_split <- function(classification_data, train_fraction = 0.70, seed = 42) {
  split_data <- classification_data
  split_data$split <- factor(rep("train", nrow(split_data)), levels = c("train", "test"))

  if (nrow(split_data) < 2) {
    return(split_data)
  }

  train_fraction <- min(max(train_fraction, 0.01), 0.99)

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)

  train_indices <- integer(0)
  test_indices <- integer(0)
  observed_classes <- unique(as.character(split_data$class))

  for (class_label in observed_classes) {
    class_indices <- which(as.character(split_data$class) == class_label)
    class_count <- length(class_indices)

    if (class_count == 0) {
      next
    }

    if (class_count == 1) {
      train_indices <- c(train_indices, class_indices)
      next
    }

    class_train_count <- floor(class_count * train_fraction)
    class_train_count <- min(max(class_train_count, 1), class_count - 1)
    class_train_indices <- sample(class_indices, size = class_train_count)

    train_indices <- c(train_indices, class_train_indices)
    test_indices <- c(test_indices, setdiff(class_indices, class_train_indices))
  }

  missing_train_classes <- setdiff(
    observed_classes,
    unique(as.character(split_data$class[train_indices]))
  )

  for (class_label in missing_train_classes) {
    candidate_index <- test_indices[as.character(split_data$class[test_indices]) == class_label][1]

    if (!is.na(candidate_index)) {
      train_indices <- c(train_indices, candidate_index)
      test_indices <- setdiff(test_indices, candidate_index)
    }
  }

  if (length(test_indices) == 0 && length(train_indices) > 1) {
    train_class_counts <- table(as.character(split_data$class[train_indices]))
    movable_classes <- names(train_class_counts[train_class_counts > 1])

    if (length(movable_classes) > 0) {
      movable_index <- train_indices[as.character(split_data$class[train_indices]) == movable_classes[1]][1]
      train_indices <- setdiff(train_indices, movable_index)
      test_indices <- c(test_indices, movable_index)
    }
  }

  split_data$split <- factor(rep("test", nrow(split_data)), levels = c("train", "test"))
  split_data$split[train_indices] <- "train"
  split_data$split <- factor(as.character(split_data$split), levels = c("train", "test"))
  split_data
}


