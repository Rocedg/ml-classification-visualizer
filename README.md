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
|   |-- playback_helpers.R
|   |-- model_dispatch_helpers.R
|   |-- data/
|   |   |-- preset_datasets.R
|   |   |-- real_datasets.R
|   |   |-- synthetic_datasets.R
|   |   |-- train_test_split.R
|   |   `-- uploaded_datasets.R
|   |-- models/
|   |   |-- logistic_regression.R
|   |   |-- knn.R
|   |   `-- svm.R
|   |-- pages/
|   |   |-- page_home.R
|   |   |-- page_theory_hub.R
|   |   `-- page_about.R
|   |-- plots/
|   |   |-- logistic_diagnostics.R
|   |   `-- main_probability_plot.R
|   `-- visualizer/
|       |-- mod_visualizer.R
|       |-- mod_visualizer_dataset_controls.R
|       |-- mod_visualizer_algorithm_controls.R
|       |-- mod_visualizer_plot_panel.R
|       |-- mod_visualizer_raw_data.R
|       |-- mod_visualizer_training_insights.R
|       |-- mod_visualizer_summary_cards.R
|       |-- mod_visualizer_interaction_panel.R
|       `-- mod_visualizer_model_explanation.R
|-- docs/
|   `-- architecture.md
|-- static/
|   |-- diabetes_health_data.csv
|   `-- titanic_passengers.csv
`-- www/
    `-- styles.css
```

## Supported models

- Logistic Regression
- SVM
- k-NN

## Preset datasets
### Synthetic datasets

- Gaussian clusters
- Linearly separable
- Overlapping classes
- Moons
- Circles

### Real-world datasets

- Titanic passengers
- Diabetes health data

## Required packages

Install these packages in R before running the app:

```r
install.packages(c("shiny", "ggplot2", "plotly", "e1071", "base64enc"))
```

The Titanic and diabetes presets are bundled as local static CSV files.

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
  - `data_helpers.R` prepares common dataset frames and plotting grids.
  - `R/data/` contains preset, uploaded, real-data, synthetic-data, and split helpers.
  - `metrics_helpers.R` calculates classification metrics.
  - `R/models/` contains the Logistic Regression, k-NN, and SVM backends.
  - `model_dispatch_helpers.R` validates run data and dispatches to the selected model.
  - `playback_helpers.R` keeps iteration navigation calculations easy to inspect.
  - `R/plots/` builds the main classification plot and model diagnostics.
- `app.R` remains the main entry point for loading packages, sourcing files, and wiring the UI and server together.
- The visualizer is designed for understanding model behavior, not for large production datasets.
