# MiteMapTools

MiteMapTools is a comprehensive R package for importing, analyzing, and visualizing movement data from MiteMap tracking systems. MiteMap is a cost-effective, open-source tool for 2D behavioral tracking of arthropods, particularly useful for studying chemotactic responses and movement patterns in controlled laboratory settings.

## About MiteMap

MiteMap is a Raspberry Pi-based automated tracking system designed to monitor arthropod behavior in circular arenas. The system uses infrared imaging to track individual organisms with high temporal resolution (position recorded every 0.2 seconds) and spatial precision. This technology enables researchers to study:

- **Chemotactic behavior**: How arthropods respond to attractive or repulsive volatile compounds
- **Movement patterns**: Analysis of trajectory complexity, speed, and spatial preferences  
- **Zone preferences**: Time allocation between different arena regions
- **Behavioral states**: Periods of activity vs. immobility

The system consists of a circular arena (typically 40mm diameter) where test subjects are placed with potential stimuli positioned at the arena periphery. High-resolution tracking data allows for detailed quantitative analysis of behavioral responses.

## Scientific Background

This package implements methods described in:

**Masier, L.‐S., Roy, L., & Durand, J.‐F. (2022). A new methodology for arthropod behavioral assays using MiteMap, a cost‐effective open‐source tool for 2D tracking. Journal of Experimental Zoology Part A: Ecological and Integrative Physiology, 337(4), 333-344.** [doi:10.1002/jez.2651](https://onlinelibrary.wiley.com/doi/10.1002/jez.2651)

The original MiteMap hardware and software can be found at: <https://github.com/LR69/MiteMap/tree/MiteMap.v6>

## Key Features

- **Data Import**: Seamlessly import MiteMap data from zip archives containing raw tracking files and metadata
- **Data Filtering**: Clean tracking data by removing artifacts, centering coordinates, and filtering by time/space constraints
- **Behavioral Analysis**: Calculate metrics like zone preferences, movement speeds, and immobility periods
- **Visualization**: Create publication-ready plots including trajectory maps, heatmaps, and statistical summaries
- **Statistical Testing**: Built-in functions for binomial tests of zone preferences with multiple comparison corrections
- **Multiple Data Formats**: Support for raw positional data and processed zone metrics (HH and CH formats)

## Data Formats

MiteMapTools handles three main types of behavioral data:

### Raw Tracking Data
High-resolution positional data with columns:
- Time (seconds, recorded every 0.2s)
- X coordinate (mm)  
- Y coordinate (mm)
- Immobility flag (boolean)

### HH Format (Half-Half)
Arena divided by a line through the odor source, computing:
- Time spent in each half
- Distance traveled in each zone
- Immobility time in each zone

### CH Format (Circle-Half)  
Arena divided by a circle centered on the odor source, encompassing half the arena surface:
- Time spent inside/outside the circle
- Movement metrics for each zone
- Behavioral preference indices

## Installation

You can install the development version of MiteMapTools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("adrientaudiere/MiteMapTools")
```

## Basic Usage

### Data Import and Filtering

This is a basic example showing how to filter and visualize MiteMap data:

``` r
library(MiteMapTools)

# Filter and center the tracking data
# center_x, center_y: adjust coordinates so odor source is at (-20, -20)
MM_filtered_centered <- filter_mitemap(MM_example, center_x = -20, center_y = -20)
#> Row removed when clearing the first secondes: 536
#> Row removed when clearing bad x range: 16860
#> Row removed when clearing bad y range: 0
#> Row removed when clearing bad x values: 24537
#> Row removed when clearing bad y values: 93
#> Row removed when clearing for run with times sup to maximum_time: 814

# Create violin plots showing position distributions by experimental condition
vioplot_mitemap(MM_filtered_centered)
```

<img src="man/figures/README-example-1.png" width="100%" />

### Data Structure Requirements

A MiteMap experiment folder should contain:

**1. Metadata file** (Excel .xlsx or CSV format) with 8 required columns:
   - **Run number**: Unique identifier for each experimental run
   - **File name**: Must match the corresponding zip file names
   - **Date**: Experiment date
   - **Start time**: Time when tracking began
   - **Farm**: Experimental farm/location identifier
   - **MiteMap number**: Device identifier (for multi-device setups)
   - **Bag**: Sample/subject container identifier
   - **Modality**: Experimental condition/treatment

**2. Zip archives** containing 4 files each:
   - **Raw data CSV**: 4-column tracking data
     - Time in seconds (recorded every 0.2s)
     - X position (mm)
     - Y position (mm)
     - Immobility flag (1 if stationary since last record)
   - **PNG heatmap**: Visual representation of movement patterns
   - **"formeD" (HH) file**: Half-half zone analysis with 8 metrics
     - Experiment metadata (date, time, run name)
     - Time spent in odor-source half vs. opposite half (seconds)
     - Immobility time in each half (seconds)
     - Distance traveled in each half (mm)
   - **"formeC" (CH) file**: Circular zone analysis with same 8-metric structure
     - Time spent inside vs. outside the circular zone
     - Movement and immobility metrics for each zone

### Arena Layout and Zone Definitions

The following diagram illustrates how the arena is divided for HH and CH analyses:

<img src="man/figures/README-shape-1.png" width="100%" />

The arena visualization shows:
- **Green regions**: HH (Half-Half) zones - arena divided by a vertical line through the odor source
- **Blue region**: CH (Circle-Half) zone - circular area centered on odor source encompassing half the arena surface
- **Red star**: Odor source location (typically positioned at arena edge)

## Advanced Usage

### Statistical Analysis

Perform binomial tests to assess zone preferences:

``` r
# Test if organisms show significant preference for odor-containing zones
results <- binom_test_mitemap(HH_example$resulting_data)
print(results)
```

### Trajectory Visualization

Create detailed trajectory plots for individual organisms:

``` r
# Plot movement paths for multiple individuals
MM_plots <- plot_ind_mitemap(MM_filtered_centered, ind_index = c(1:6))

# Combine plots with shared color scale
library(patchwork)
(MM_plots[[1]] + MM_plots[[2]] + MM_plots[[3]]) /
  (MM_plots[[4]] + MM_plots[[5]] + MM_plots[[6]]) +
  plot_layout(guides = "collect")
```

### Heatmap Generation

Generate movement density heatmaps:

``` r
# Create heatmaps for an entire experimental folder
make_heatmap_from_multiple_folders(
  folders = c("path/to/experiment1", "path/to/experiment2"),
  output_folder = "path/to/heatmaps"
)
```

## Package Functions

### Core Functions

- `import_mitemap()`: Import data from MiteMap zip files and metadata
- `filter_mitemap()`: Clean and filter tracking data
- `plot_ind_mitemap()`: Create individual trajectory visualizations
- `vioplot_mitemap()`: Generate violin plots of positional distributions
- `binom_test_mitemap()`: Perform statistical tests on zone preferences

### Analysis Functions

- `convex_hull_mitemap()`: Calculate convex hull metrics for movement analysis
- `make_heatmap()`: Generate movement density heatmaps
- `make_heatmap_from_multiple_folders()`: Batch heatmap generation

### Utility Functions

- `import_mitemap_from_multiple_folder()`: Import data from multiple experiment folders
- `rename_files_with_number()`: Standardize file naming conventions

## Contributing

We welcome contributions to MiteMapTools! Please see our contribution guidelines and feel free to submit issues or pull requests.

## Citation

If you use MiteMapTools in your research, please cite:

Masier, L.‐S., Roy, L., & Durand, J.‐F. (2022). A new methodology for arthropod behavioral assays using MiteMap, a cost‐effective open‐source tool for 2D tracking. Journal of Experimental Zoology Part A: Ecological and Integrative Physiology, 337(4), 333-344. doi:10.1002/jez.2651

## License

This package is licensed under the MIT License. See LICENSE.md for details.