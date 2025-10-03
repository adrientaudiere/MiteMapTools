---
title: 'MiteMapTools: An R Package for Analyzing Arthropod Movement Data from MiteMap Tracking Systems'
tags:
  - R
  - behavioral ecology
  - arthropod tracking
  - chemotaxis
  - movement analysis
authors:
  - name: Adrien Taudière
    orcid: 0000-0003-1088-1182
    affiliation: 1
  - name: Lise Roy
    orcid: 0000-0001-8833-1717
    affiliation: 2
affiliations:
 - name: IdEst, Saint-Bonnet-de-Salendrinque, France
   index: 1
 - name: Centre d'Ecologie Fonctionnelle et Evolutive (CEFE), CNRS, Montpellier, France
   index: 2
date: 3 October 2025
bibliography: paper.bib
---

# Summary

`MiteMapTools` is a comprehensive R package designed for importing, analyzing, and visualizing movement data from MiteMap tracking systems. MiteMap is a cost-effective, [open-source](https://github.com/LR69/MiteMap/tree/MiteMap.v6) Raspberry Pi-based system for 2D behavioral tracking of arthropods [@masier2022]. The package provides a complete workflow for processing high-resolution tracking data from circular arena experiments, enabling researchers to study chemotactic responses, movement patterns, and spatial responses to volatile compounds in mites.

# Statement of Need

Behavioral ecology research increasingly relies on automated tracking systems to quantify animal movement and responses to environmental stimuli [@dell2014]. The MiteMap system already recorded the movement of mite in an arena using image tracking of individual mite[@masier2022]. However, it generate raw data that are challenging to interpret and requires robust methods to crystalize this information into tractable and meaningful statistics [@dell2014]. Thus the `MiteMapTools` R package aims to complete the MiteMap system by providing:

- **Standardized data import**: Seamless integration of raw tracking files and experimental metadata
- **Quality control**: Functions for filtering artifacts (e.g., the first seconds of recording, point outside the arena boundaries)
- **Behavioral metrics**: Automated calculation of numerous metrics including zone preferences regarding odor sources, movement speeds, turning angles, and path crossing events.
- **Statistical analysis**: Built-in hypothesis testing for spatial preferences with multiple comparison corrections
- **Data visualizations**: Trajectory maps and statistical summaries

MiteMapTools relies on well-established R ecosystem tidyverse [@wickham2019] as well as `ggplot2` [@wickham2016] and ggstats [@patil2021] for visualizations and statistical testing. MiteMapTools is adapt to indididual tracking in a circular arena with only one odor source. For more complex experimental designs (e.g., multiple odor sources, rectangular arenas, group tracking), other R packages such as `swaRmverse` [papadopoulou2025] or `trajr` [@mclean2018], may be more appropriate (see [tracking R taskview](https://cran.r-project.org/web/views/Tracking.html)).

# Key Features
## Data Import and Preprocessing


## Behavioral Metrics Calculation

## Exploratory Data Analysis

## Statistical Testing


# Acknowledgements


# References