#' MiteMapTools: A toolkit for analyzing arthropod movement data
#'
#' @description
#' MiteMapTools provides a comprehensive suite of functions for importing, processing, 
#' and analyzing movement data from MiteMap tracking systems. MiteMap is a Raspberry 
#' Pi-based automated tracking system designed to monitor arthropod behavior in 
#' controlled 2D arenas.
#' 
#' @section Background:
#' The MiteMap system consists of a circular arena (typically 40mm diameter) where 
#' arthropods are tracked using infrared imaging. The arena can be configured with 
#' attractive or repulsive stimuli (e.g., volatile compounds) to study chemotactic 
#' behavior. Position data is recorded at high temporal resolution (every 0.2 seconds).
#' 
#' @section Data Types:
#' The package handles three main data formats:
#' \itemize{
#'   \item \strong{Raw data}: Time-series of x,y coordinates with immobility flags
#'   \item \strong{HH format}: Time spent in arena halves (left/right of odor source)
#'   \item \strong{CH format}: Time spent in circular zones around the odor source
#' }
#' 
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{import_mitemap}}: Import data from zip files and metadata
#'   \item \code{\link{filter_mitemap}}: Clean and filter tracking data
#'   \item \code{\link{plot_ind_mitemap}}: Create trajectory visualizations
#'   \item \code{\link{vioplot_mitemap}}: Generate distribution plots
#'   \item \code{\link{binom_test_mitemap}}: Perform statistical tests on zone preferences
#'   \item \code{\link{make_heatmap}}: Create movement heatmaps
#' }
#' 
#' @section References:
#' Masier, L.‐S., Roy, L., & Durand, J.‐F. (2022). A new methodology for arthropod 
#' behavioral assays using MiteMap, a cost‐effective open‐source tool for 2D tracking. 
#' Journal of Experimental Zoology Part A: Ecological and Integrative Physiology, 
#' 337(4), 333-344. \doi{10.1002/jez.2651}
#' 
#' @name MiteMapTools-package
#' @aliases MiteMapTools
#' @docType package
#' @author Adrien Taudière \email{adrien.taudiere@@zaclys.net}
#' @author Lise Roy \email{lise.roy@@cefe.cnrs.fr}
#' @import tidyverse readxl
#' @keywords package
NULL


if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "%>%", ".data", "Binary_choice", "Din.mm.", "Dout.mm.", "Farm", "File_name", "Modality", "Modality_interm", "Time_total", "Tin.s.", "Tin_m.s.", "Tout.s.", "Tout_m.s.", "X..t.s.", "across", "add_column", "aes", "bind_rows", "binom.test", "chull", "distinct", "everything", "facet_wrap", "geom_violin", "ggplot", "ggtitle", "group_by", "if_else", "inner_join", "is_tibble", "lines", "mutate", "n", "na.omit", "p.adjust", "points", "prop_Tin", "quantile", "ratio_choice", "read.csv", "read.delim", "read_excel", "rgb", "tibble", "title", "unzip", "x.mm.", "y.mm.", "yes"
  ))
}

#' @keywords internal
#' @noRd
"_PACKAGE"

## usethis namespace: start
#' @importFrom grDevices chull rgb
#' @importFrom graphics lines points title
#' @importFrom stats binom.test na.omit p.adjust quantile
#' @importFrom utils read.csv read.delim unzip
## usethis namespace: end
NULL
