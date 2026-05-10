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
|   |-- plot_helpers.R
|   |-- pages/
|   |   |-- page_home.R
|   |   |-- page_theory_hub.R
|   |   `-- page_about.R
|   `-- visualizer/
|       |-- mod_visualizer.R
|       |-- mod_visualizer_dataset_controls.R
|       |-- mod_visualizer_algorithm_controls.R
|       |-- mod_visualizer_plot_panel.R
|       |-- mod_visualizer_raw_data.R
|       `-- mod_visualizer_model_explanation.R
|-- docs/
|   `-- architecture.md
`-- www/
    `-- styles.css
```

## Supported models

- Logistic Regression
- SVM (coming soon in the UI)
- k-NN (coming soon in the UI)

## Preset datasets
## Real-world datasets

The app includes real-world datasets adapted to the required 2D format:

- **Titanic passengers**: uses passenger age and ticket fare to classify whether a passenger survived.
- **Diabetes health data**: uses glucose level and body mass index to classify diabetes outcome.

These datasets are simplified for visualization. The original datasets contain more variables, but ML Visualizer uses only two numeric variables so the decision regions can be shown clearly.
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
install.packages(c("shiny", "ggplot2", "e1071", "titanic", "mlbench"))
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
