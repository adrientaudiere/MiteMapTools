#' @importFrom conflicted conflicts_prefer
#' @import dplyr
.onLoad <- function(libname, pkgname) {
  # Package loading code here
  conflicted::conflicts_prefer(
    dplyr::filter,
    testthat::local_edition,
    testthat::edition_get,
    dplyr::select,
    dplyr::lag,
    dplyr::matches,
    .quiet = TRUE
  )
}
