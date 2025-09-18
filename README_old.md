
# MiteMapTools

The goal of MiteMapTools is to …

[Masier et
al. 2022](https://onlinelibrary.wiley.com/doi/10.1002/jez.2651)

<https://github.com/LR69/MiteMap/tree/MiteMap.v6>

## Installation

You can install the development version of MiteMapTools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("adrientaudiere/MiteMapTools")
```

## Basic Usage

### Data Import and Filtering

## Example

This is a basic example showing how to filter and visualize MiteMap data:

``` r
library(MiteMapTools)

# Filter and center the tracking data
# center_x, center_y: adjust coordinates so odor source is at (-20, -20)
MM_filtered_centered <- filter_mitemap(MM_example, center_x = -20, center_y = -20)

# Create violin plots showing position distributions by experimental condition
vioplot_mitemap(MM_filtered_centered)
```
#> Le chargement a nécessité le package : tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> Le chargement a nécessité le package : readxl

MM_filtered_centered <- filter_mitemap(MM_example, center_x = -20, center_y = -20)
#> Row removed when clearing the first secondes: 536
#> Row removed when clearing bad x range: 16860
#> Row removed when clearing bad y range: 0
#> Row removed when clearing bad x values: 24537
#> Row removed when clearing bad y values: 93
#> Row removed when clearing for run with times sup to maximum_time: 814
vioplot_mitemap(MM_filtered_centered)
```

<img src="man/figures/README-example-1.png" width="100%" />

A input folder for MiteMapTools consist of a - a metadata file (in xlsx
or csv) with 8 columns: - Run number - File name (must be present in the
list of zip files) - Date - Start time - Farm - MiteMap number - Bag -
Modality

- a list of zip files with 4 files compressed inside
  - A raw data file with 4 columns
    - The time in second (position is recorded every 0.2s)
    - The position in x (in mm)
    - The position in y (in mm)
    - Boolean variable indicating if the individual has remained
      immobile since the last record (1 if immobile)
  - A png file figuring the position of the individual using an heatmap
  - A processed data file called “formeD” (also called HH) compute the
    time spent in an area defined by the half side of the arena (see
    figure @ref(fig:cars-plot)).
    - The date and hour of the experiment
    - The name of the MiteMap run
    - Total time spent in the half containing the odor source (second)
    - Total time spent in the opposite half (second)
    - Time spent immobile in the half containing the odor source
      (second)
    - Time spent immobile in the opposite half (second)
    - Total distance traveled in the half containing the odor source
      (mm)
    - Total distance traveled in the opposite half (mm)
  - A processed data file called “formeC” (also called CH) compute the
    time spent in an area defined by a circle center on the odor source
    (see figure @ref(fig:cars-plot)) encompassing half of the arena
    surface.
    - The date and hour of the experiment
    - The name of the MiteMap run
    - Total time spent in the half containing the odor source (second)
    - Total time spent in the opposite half (second)
    - Time spent immobile in the half containing the odor source
      (second)
    - Time spent immobile in the opposite half (second)
    - Total distance traveled in the half containing the odor source
      (mm)
    - Total distance traveled in the opposite half (mm)

<img src="man/figures/README-shape-1.png" width="100%" />
