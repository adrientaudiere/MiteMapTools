#' \code{MiteMap} package
#'
#' Functions to import and analyze MiteMap.
#'
#' @name MiteMapTools-package
#' @import tidyverse readxl
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
