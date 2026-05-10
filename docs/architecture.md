# Architecture

The app has four top-level pages:

- Home
- Visualizer
- Model Theory Hub
- About Us

The Visualizer page owns these internal submodules:

- Dataset Controls
- Algorithm Controls
- Plot Panel
- Raw Data Panel
- Model Explanation Panel

The Visualizer module coordinates:

- `base_classification_data`
- `drawn_classification_data`
- `current_classification_data`
- `trained_model_bundle`

```mermaid
graph TD
    App[app.R]

    App --> Home[Page: Home]
    App --> Visualizer[Page: Visualizer]
    App --> TheoryHub[Page: Model Theory Hub]
    App --> About[Page: About Us]

    Visualizer --> DatasetControls[Visualizer: Dataset Controls]
    Visualizer --> AlgorithmControls[Visualizer: Algorithm Controls]
    Visualizer --> PlotPanel[Visualizer: Plot Panel]
    Visualizer --> RawData[Visualizer: Raw Data Panel]
    Visualizer --> ModelExplanation[Visualizer: Model Explanation Panel]
```
