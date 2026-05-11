# Refactor Plan

This refactor keeps app behavior stable while making future algorithm work easier.

Completed organization:

- model backends moved into `R/models/`
- dataset generation/loading/splitting moved into `R/data/`
- plot construction split into `R/plots/`
- sidebar parameter UI, interaction panel UI, summary cards, and Training insights UI split into focused visualizer files

Next safe places to work:

- k-NN polish: `R/models/knn.R`, `R/visualizer/mod_visualizer_interaction_panel.R`, and `R/plots/main_probability_plot.R`
- k-NN parameters: `R/visualizer/mod_visualizer_parameters.R`, `R/models/knn.R`, and `R/model_dispatch_helpers.R`
- future SVM backend: new `R/models/svm.R`, then add selection/routing in `R/model_dispatch_helpers.R` and the visualizer parameter module
