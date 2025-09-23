# Version 0.1.0 

## New Features

- Change behavior of [summarize_mitemap()] to return mean, max, min and sd value
for all numeric columns. Add also parameter `selected_cols` to select specific
columns to summarize.

# Version 0.0.2

## Improvements 

- Removed HH and CH params from [filter_mitemap()].
- Computed HH and CH formats directly in R inside [import_mitemap()].

# Version 0.0.1

## New Features
* Initial release of MiteMapTools package
* **Data Import**: Functions to import MiteMap data from zip archives and metadata files ([import_mitemap()], [import_mitemap_from_multiple_folder()])
* **Data Processing**: Filtering and cleaning of tracking data ([filter_mitemap()])
* **Visualization**: 
  - Individual trajectory plotting ([plot_ind_mitemap()])
  - Violin plots for position distributions ([vioplot_mitemap()])
  - Movement heatmap generation ([extract_heatmap()])
* **Statistical Analysis**: Binomial tests for zone preference analysis ([binom_test_mitemap()])
* **Geometric Analysis**: Convex hull calculations for movement characterization ([convex_hull_mitemap()])

## Data Format Support
* Raw tracking data (x,y coordinates with temporal resolution of 0.2s)
* HH format (Half-Half zone analysis)
* Comprehensive metadata integration

## Documentation
* Complete package documentation with biological context
* Detailed examples and use cases
* Integration with Masier et al. (2022) methodology
* Connection to original MiteMap hardware project (LR69/MiteMap)

## Dependencies
* Built on tidyverse ecosystem for data manipulation
* Supports Excel and CSV metadata formats via readxl
* Compatible with ggplot2 for publication-ready visualizations
