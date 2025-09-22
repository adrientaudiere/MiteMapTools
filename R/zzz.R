#' @importFrom conflicted conflicts_prefer
#' @importFrom purrr is_null
#' @import dplyr
.onLoad <- function(libname, pkgname) {
  # Package loading code here
  conflicted::conflicts_prefer(
    dplyr::filter,
    purrr::is_null,
    testthat::local_edition,
    testthat::edition_get,
    dplyr::select,
    dplyr::lag,
    dplyr::matches,
    .quiet = TRUE
  )
}

