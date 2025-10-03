---
title: 'MiteMapTools: An R package for analyzing arthropod movement data from MiteMap tracking systems'
tags:
  - R
  - behavioral ecology
  - arthropods
  - movement analysis
  - chemotaxis
  - tracking data
authors:
  - name: Adrien Taudière
    orcid: 0000-0003-1088-1182
    corresponding: true
    affiliation: 1
  - name: Lise Roy
    orcid: 0000-0001-8833-1717
    affiliation: 2
affiliations:
 - name: Independent Researcher, France
   index: 1
 - name: CEFE, Univ Montpellier, CNRS, EPHE, IRD, Montpellier, France
   index: 2
date: 3 October 2024
bibliography: paper.bib
---

# Summary

MiteMapTools is a comprehensive R package for importing, analyzing, and visualizing movement data from MiteMap tracking systems. MiteMap is a cost-effective, open-source Raspberry Pi-based tool designed for 2D behavioral tracking of arthropods in controlled laboratory settings [@Masier2022]. The system uses infrared imaging to track individual organisms with high temporal resolution (position recorded every 0.2 seconds) and spatial precision in circular arenas (typically 40mm diameter). MiteMapTools provides researchers with a complete workflow for processing raw tracking data, computing behavioral metrics, and creating publication-ready visualizations to study chemotactic responses and movement patterns.

# Statement of need

Behavioral ecology research increasingly relies on automated tracking systems to quantify animal movement and decision-making. While the MiteMap hardware system [@Masier2022] provides an accessible platform for high-resolution arthropod tracking, researchers needed dedicated software tools to efficiently process and analyze the resulting data. Existing tracking analysis packages often focus on vertebrate movement or require proprietary software, creating barriers for arthropod behavioral research.

MiteMapTools addresses this gap by providing specialized functions tailored to the MiteMap system's output format and common experimental designs in arthropod behavioral ecology. The package enables researchers to:

1. Import and integrate tracking data with experimental metadata
2. Clean and filter trajectories to remove artifacts
3. Compute zone-based metrics (time allocation, distance traveled, immobility periods)
4. Perform statistical analyses of spatial preferences
5. Generate publication-ready visualizations of movement patterns

The package has been designed with a focus on chemotaxis experiments, where researchers test arthropod responses to volatile chemical compounds. However, its flexible design makes it applicable to any 2D tracking experiment studying spatial preferences, movement patterns, or behavioral states in small arthropods.

# Key Features

MiteMapTools provides several categories of functionality:

**Data Import and Management**: The `import_mitemap()` function seamlessly imports MiteMap data from zip archives containing raw CSV tracking files and PNG heatmaps, automatically integrating experimental metadata from Excel or CSV files. The package handles file matching, data validation, and formats the data into tidy data structures compatible with the tidyverse ecosystem [@Wickham2019].

**Data Processing and Filtering**: The `filter_mitemap()` function cleans tracking data by removing initial adjustment periods, centering coordinates on the arena, and filtering out-of-bounds positions. This preprocessing step is crucial for ensuring accurate behavioral metrics and is automatically applied during import when requested.

**Behavioral Metrics**: The package computes two standard zone formats used in arthropod behavioral research:
- **HH format (Half-Half)**: Divides the arena by a line through the odor source, computing time spent in each half, distance traveled, and immobility time
- **CH format (Circle-Half)**: Divides the arena by a circle centered on the odor source, encompassing half the arena surface, enabling analysis of attraction or repulsion behaviors

Additional metrics include movement speed, turning angles, distance from stimuli, and convex hull analysis for characterizing spatial usage patterns (`convex_hull_mitemap()`).

**Statistical Analysis**: Built-in functions for binomial tests of zone preferences (`binom_test_mitemap()`) with multiple comparison corrections enable rigorous statistical evaluation of behavioral responses across experimental treatments.

**Visualization**: Publication-ready plotting functions include:
- Individual trajectory maps colored by movement speed (`plot_ind_mitemap()`)
- Violin plots showing position distributions across experimental conditions (`vioplot_mitemap()`)
- Heatmap extraction and visualization (`extract_heatmap()`)
- Integrated support for ggplot2 [@Wickham2016] enabling custom visualizations

# Research Applications

MiteMapTools has been developed to support research on arthropod behavioral ecology, particularly studies of chemotactic responses in mites and other small arthropods. The package is currently being used to analyze data from experiments investigating:

- Host-seeking behavior in parasitic mites
- Responses to plant volatile compounds in predatory mites
- Olfactory preferences in biological control agents
- Effects of environmental factors on movement patterns

The methodology implemented in this package builds directly on the work of @Masier2022, who validated the MiteMap system for studying poultry red mite (*Dermanyssus gallinae*) behavior. By providing accessible tools for data analysis, MiteMapTools lowers the barrier to entry for behavioral ecology research and promotes reproducibility through standardized analytical workflows.

# Acknowledgements

We thank Jean-François Durand for his contribution to the development of the MiteMap hardware system. This work builds upon the methodology described in @Masier2022.

# References
