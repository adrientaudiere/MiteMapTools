---
title: 'MiteMapTools: An R Package for Analyzing Arthropod Movement Data from MiteMap Tracking Systems'
tags:
  - R
  - behavioral ecology
  - arthropod tracking
  - chemotaxis
  - movement analysis
  - data visualization
authors:
  - name: Adrien Taudière
    orcid: 0000-0003-1088-1182
    affiliation: 1
  - name: Lise Roy
    orcid: 0000-0001-8833-1717
    affiliation: 1
affiliations:
 - name: Centre d'Ecologie Fonctionnelle et Evolutive (CEFE), CNRS, Montpellier, France
   index: 1
date: 3 October 2025
bibliography: paper.bib
---

# Summary

`MiteMapTools` is a comprehensive R package designed for importing, analyzing, and visualizing movement data from MiteMap tracking systems. MiteMap is a cost-effective, open-source Raspberry Pi-based system for 2D behavioral tracking of arthropods [@Masier2022]. The package provides a complete workflow for processing high-resolution tracking data (temporal resolution of 0.2 seconds) from circular arena experiments, enabling researchers to study chemotactic responses, movement patterns, and spatial preferences in arthropods and other small organisms.

# Statement of Need

Behavioral ecology research increasingly relies on automated tracking systems to quantify animal movement and responses to environmental stimuli. While MiteMap hardware provides an accessible solution for 2D tracking [@Masier2022], researchers need robust software tools to process and analyze the resulting data efficiently. `MiteMapTools` addresses this gap by providing:

- **Standardized data import**: Seamless integration of raw tracking files and experimental metadata
- **Quality control**: Functions for filtering artifacts and centering coordinates
- **Behavioral metrics**: Automated calculation of zone preferences, movement speeds, and immobility periods
- **Statistical analysis**: Built-in hypothesis testing for spatial preferences with multiple comparison corrections
- **Publication-ready visualizations**: Trajectory maps, heatmaps, and statistical summaries

The package implements the methodology described in @Masier2022 while extending it with additional analytical capabilities. It is particularly valuable for researchers studying chemotaxis, host-seeking behavior, and movement ecology in arthropods, where quantitative analysis of spatial preferences is critical.

# Key Features

`MiteMapTools` supports two primary zone analysis formats:

- **HH Format (Half-Half)**: Arena divided by a line through the stimulus source
- **CH Format (Circle-Half)**: Arena divided by a circle centered on the stimulus source

The package provides functions for:

- Data import from zip archives containing raw CSV tracking data and PNG heatmaps
- Trajectory filtering and coordinate transformation
- Calculation of behavioral metrics (time in zones, distance traveled, immobility periods)
- Visualization tools including individual trajectory plots and violin plots
- Statistical testing with binomial tests for zone preference analysis

# Acknowledgements

We acknowledge the original developers of the MiteMap hardware system and thank the behavioral ecology community for feedback during package development.

# References
