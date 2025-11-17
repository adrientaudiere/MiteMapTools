# Changelog

## Version 0.1.1

### New Features

- The `factor` parameter of function
  [`binom_test_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/binom_test_mitemap.md)
  now accepts also vector of names to enable the analysis of multiple
  factors.

- The `factor` parameter of function
  [`extract_heatmap()`](https://adrientaudiere.github.io/MiteMapTools/reference/extract_heatmap.md)
  now accepts also vector of names to enable the analysis of multiple
  factors. The factor are first collapsed into a new combined factor,
  the order of the factor is important.

## Version 0.1.0

### New Features

- Change behavior of
  [`summarize_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/summarize_mitemap.md)
  to return mean, max, min and sd value for all numeric columns. Add
  also parameter `selected_cols` to select specific columns to
  summarize.

- Add `cli` messages and progress bar.

## Version 0.0.2

### Improvements

- Removed HH and CH params from
  [`filter_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/filter_mitemap.md).
- Computed HH and CH formats directly in R inside
  [`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md).

## Version 0.0.1

### New Features

- Initial release of MiteMapTools package
- **Data Import**: Functions to import MiteMap data from zip archives
  and metadata files
  ([`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md),
  [`import_mitemap_from_multiple_folder()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap_from_multiple_folder.md))
- **Data Processing**: Filtering and cleaning of tracking data
  ([`filter_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/filter_mitemap.md))
- **Visualization**:
  - Individual trajectory plotting
    ([`plot_ind_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/plot_ind_mitemap.md))
  - Violin plots for position distributions
    ([`vioplot_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/vioplot_mitemap.md))
  - Movement heatmap generation
    ([`extract_heatmap()`](https://adrientaudiere.github.io/MiteMapTools/reference/extract_heatmap.md))
- **Statistical Analysis**: Binomial tests for zone preference analysis
  ([`binom_test_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/binom_test_mitemap.md))
- **Geometric Analysis**: Convex hull calculations for movement
  characterization
  ([`convex_hull_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/convex_hull_mitemap.md))

### Data Format Support

- Raw tracking data (x,y coordinates with temporal resolution of 0.2s)
- HH format (Half-Half zone analysis)
- Comprehensive metadata integration

### Documentation

- Complete package documentation with biological context
- Detailed examples and use cases
- Integration with Masier et al. (2022) methodology
- Connection to original MiteMap hardware project (LR69/MiteMap)

### Dependencies

- Built on tidyverse ecosystem for data manipulation
- Supports Excel and CSV metadata formats via readxl
- Compatible with ggplot2 for publication-ready visualizations
