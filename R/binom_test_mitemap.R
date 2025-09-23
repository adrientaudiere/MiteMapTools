#' Test binomial on MiteMap for presence in the half part of the odor
#'
#' @details
#'  The test is run for each factor values with a p-adjustement step. For each
#'  run (filename), the proportion of points in the left half is calculated. If the
#'  proportion is superior to 0.5, the run is considered as "in left", else "in right".
#'  Then a binomial test is run to test if the proportion of runs "in left" is
#'  significantly different from 0.5 for each levels of the factor. If format is "CH",
#'  the same process is done but for presence in the circular half rather than
#'  a half part.
#'
#'  If level is "lines", each line of MiteMap is considered as one replicate
#'  (i.e. the proportion of points in left half is calculated for each line of MiteMap).
#'  This approach is not recommended as it introduce a high pseudoreplication risk.
#'
#' @param MiteMap (required) The result of import_mitemap
#' @param factor (required, default NULL) The column name to separate individuals
#' in the MiteMap data frame (e.g., "Treatment").
#' @param format (default "HH") The format of `left` area. "HH" for Half-Half,
#' "CH" for Circular-Half.
#' @param verbose (Logical, default = TRUE) If TRUE, the function print additional
#'  information.
#' @param p.adjust_method (default "BH") method for p-adjustement.
#'   See [stats::p.adjust()].
#' @param level (default "run") The level of analysis. "run" to consider each run
#'  (File_name) as one replicate. "lines" to consider each line
#'  (i.e. each temporal point) of MiteMap as one replicate. The level run is
#'  more conservative as it considers that each run is one independent replicate.
#'  Use level "lines" carefully as it introduce a high pseudoreplication risk.
#' @param alternative (default "two.sided") The alternative hypothesis to test.
#'  See [stats::binom.test()].
#'
#' @return A tibble of results for binomial test
#'
#' @export
#' @author Adrien Taudière
#' @examples
#'
#'
#' binom_test_mitemap(MM_data, factor = "Treatment")
#' binom_test_mitemap(MM_data, factor = "Treatment", format = "CH")
#' binom_test_mitemap(MM_data, factor = "Treatment", level = "lines")
#'
#' MM_data |>
#'   filter(Biomol_sp %in% c("DGSS", "DGL1", "D_carpathicus")) |>
#'   binom_test_mitemap(factor = "Biomol_sp")
binom_test_mitemap <- function(MiteMap,
                               factor = NULL,
                               format = "HH",
                               verbose = TRUE,
                               p.adjust_method = "BH",
                               level = "run",
                               alternative = "two.sided") {
  if (verbose) {
    MM_ind <- summarize_mitemap(MiteMap)
  } else {
    MM_ind <- suppressWarnings(summarize_mitemap(MiteMap))
  }

  if (format == "HH") {
    MM_ind <- MM_ind |>
      mutate(
        in_left_TRUE = in_left_half_HH_nb_TRUE,
        in_left_FALSE = in_left_half_HH_nb_FALSE,
        in_left_prop_TRUE = in_left_TRUE / (in_left_TRUE + in_left_FALSE),
        in_left_prop_TRUE = in_left_FALSE / (in_left_TRUE + in_left_FALSE),
        in_left = in_left_prop_TRUE > 0.5,
        in_right = in_left_prop_TRUE <= 0.5
      )
  } else if (format == "CH") {
    MM_ind <- MM_ind |>
      mutate(
        in_left_TRUE = in_left_half_CH_nb_TRUE,
        in_left_FALSE = in_left_half_CH_nb_FALSE,
        in_left_prop_TRUE = in_left_TRUE / (in_left_TRUE + in_left_FALSE),
        in_left_prop_TRUE = in_left_FALSE / (in_left_TRUE + in_left_FALSE),
        in_left = in_left_prop_TRUE > 0.5,
        in_right = in_left_prop_TRUE <= 0.5
      )
  } else {
    stop("format must be 'HH' or 'CH'")
  }

  if (level == "run") {
    MM_ind <- MM_ind |>
      group_by(.data[[factor]]) |>
      summarise(
        n = n(),
        yes = sum(in_left, na.rm = TRUE),
        no = sum(in_right, na.rm = TRUE)
      )
  } else if (level == "lines") {
    MM_ind <- MM_ind |>
      group_by(.data[[factor]]) |>
      summarise(
        n = n(),
        yes = sum(in_left_TRUE, na.rm = TRUE),
        no = sum(in_left_FALSE, na.rm = TRUE)
      )
  } else {
    stop("Paramter `level` must be 'run' or 'lines'")
  }

  CI <- apply(MM_ind, 1, function(xx) {
    paste(
      round(
        binom.test(
          as.integer(xx["yes"]),
          as.integer(xx["no"]) + as.integer(xx["yes"]),
          alternative = alternative
        )$conf.int[1],
        3
      ),
      round(
        binom.test(
          as.integer(xx["yes"]),
          as.integer(xx["no"]) + as.integer(xx["yes"]),
          alternative = alternative
        )$conf.int[2],
        3
      ),
      sep = " - "
    )
  })

  estimate <- apply(MM_ind, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = alternative
      )$estimate,
      3
    )
  })

  p.value <- apply(MM_ind, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = alternative
      )$p.value,
      3
    )
  })

  p.value[p.value == 0] <- 0.00001
  p.value.adj <- p.adjust(p.value, method = p.adjust_method)

  New_MM_ind <-
    MM_ind |> add_column(
      p.value = p.value,
      p.value.adj = p.value.adj,
      estimate = estimate,
      CI = CI
    )
  return(New_MM_ind)
}
