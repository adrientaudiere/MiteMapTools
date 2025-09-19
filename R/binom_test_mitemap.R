#' Test binomial on MiteMap for presence in the half part of the odor
#' 
#' @details 
#'   The test is run for each factor values with a p-adjustement step.
#'
#' @param MiteMap (required) The result of import_mitemap
#' @param factor The column name to separate test
#' @param p.adjust_method method for p-adjustement. See [p.adjust()]
#' @return A tibble of results for binomial test
#'
#' @export
#' @author Adrien Taudière
#' @examples
#' binom_test_mitemap(MM_ind_data, factor = "Modality")
#' 
#' MM_ind_data |>
#'   mutate(Farm_factor=paste(Farm, Modality)) |>
#'     binom_test_mitemap(factor = "Farm_factor", 
#'     p.adjust_method="bonferroni")
binom_test_mitemap <- function(MiteMap, factor = NULL, p.adjust_method = "BH") {
  MiteMap_bin <- MiteMap |>
    mutate(Time_total = Tin.s. + Tout.s.) |>
    mutate(ratio_choice = Tin.s. / Time_total) |>
    mutate(Prop_immobile = 1 - ((Tin_m.s. + Tout_m.s.) / Time_total)) |>
    mutate(speed = (Din.mm. + Dout.mm.) / Time_total) |>
    dplyr::filter(Time_total < 603) |>
    dplyr::filter(ratio_choice <= 1 & ratio_choice >= 0) |>
    mutate(Binary_choice = if_else(ratio_choice > 0.5, 1, 0, NA_real_)) |>
    mutate(factor = factor(factor)) |>
    dplyr::group_by(.data[[factor]]) |>
    dplyr::summarise(
      n = n(),
      HH = mean(Binary_choice),
      yes = sum(Binary_choice),
      no = n - yes
    )

  CI <- apply(MiteMap_bin, 1, function(xx) {
    paste(
      round(
        binom.test(
          as.integer(xx["yes"]),
          as.integer(xx["no"]) + as.integer(xx["yes"]),
          alternative = "two.sided"
        )$conf.int[1],
        3
      ),
      round(
        binom.test(
          as.integer(xx["yes"]),
          as.integer(xx["no"]) + as.integer(xx["yes"]),
          alternative = "two.sided"
        )$conf.int[2],
        3
      ),
      sep = " - "
    )
  })

  estimate <- apply(MiteMap_bin, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$estimate,
      3
    )
  })

  p.value <- apply(MiteMap_bin, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$p.value,
      3
    )
  })

  p.value[p.value == 0] <- 0.001
  p.value.adj <- p.adjust(p.value, method = p.adjust_method)

  NewMiteMap_bin <-
    MiteMap_bin |> add_column(
      p.value = p.value,
      p.value.adj = p.value.adj,
      estimate = estimate,
      CI = CI
    )
  return(NewMiteMap_bin)
}
