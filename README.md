# ML Visualizer

ML Visualizer is a beginner-friendly R Shiny application for learning how classification models behave on simple 2D datasets.

The app focuses only on classification workflows:

1. Select or create a dataset
2. Choose a classifier
3. Run the model
4. Study the decision regions, raw data, and metrics

## Project structure

```text
myDashboardApp/
|-- app.R
|-- R/
|   |-- data_helpers.R
|   |-- metrics_helpers.R
|   |-- model_training_helpers.R
|   |-- playback_helpers.R
|   `-- plot_helpers.R
|-- modules/
|   |-- home_module.R
|   |-- visualizer_module.R
|   |-- theory_hub_module.R
|   |-- about_module.R
|   |-- dataset_controls_module.R
|   |-- algorithm_controls_module.R
|   |-- plot_panel_module.R
|   |-- raw_data_module.R
|   `-- model_theory_panel_module.R
`-- www/
    `-- styles.css
```

## Supported models

- Logistic Regression
- SVM (coming soon in the UI)
- k-NN (coming soon in the UI)

## Preset datasets

- Gaussian clusters
- Linearly separable
- Overlapping classes
- Moons
- Circles

## Required packages

Install these packages in R before running the app:

```r
install.packages(c("shiny", "ggplot2", "e1071"))
```

The `class` package is normally included with R, and it is used for k-NN.

## How to run the app

Open R or RStudio, set your working directory to `myDashboardApp`, and run:

```r
shiny::runApp()
```

## CSV format for uploaded data

Your CSV file must contain exactly these columns:

- `x`
- `y`
- `class`

Example:

```csv
x,y,class
-2.1,1.7,A
-1.8,2.4,A
2.3,-1.5,B
1.9,-2.2,B
```

The app automatically converts the two uploaded class labels into `Class A` and `Class B`.

## Beginner notes

- The project is split into small modules so the code is easier to read.
- Shared helper functions are kept in the `R/` folder:
  - `data_helpers.R` prepares preset, uploaded, and plotting-grid data.
  - `metrics_helpers.R` calculates classification metrics.
  - `model_training_helpers.R` trains the currently available Logistic Regression model.
  - `playback_helpers.R` keeps iteration navigation calculations easy to inspect.
  - `plot_helpers.R` builds the main classification plot and iteration metric plot.
- `app.R` remains the main entry point for loading packages, sourcing files, and wiring the UI and server together.
- The visualizer is designed for understanding model behavior, not for large production datasets.
