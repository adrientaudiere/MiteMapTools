#' Make violin plot of MiteMap data
#'
#' @param MiteMap (required) The result of import_mitemap ($resulting_data) for raw_data
#' @param modality : A name of column present in the MiteMap to separate violin plot.
#' @param wrap : A name of column present in the MiteMap to wrap violin plot.
#'
#' @return A ggplot object
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' MM_filtered_centered <- filter_mitemap(MM_data, center_x = -20, center_y = -20)
#' vioplot_mitemap(MM_filtered_centered)
#' vioplot_mitemap(MM_data$resulting_data, modality = "Farm", wrap = "Modality") +
#'   geom_boxplot(alpha = 0.1)
vioplot_mitemap <- function(MiteMap,
                            modality = "Modality",
                            wrap = NULL) {
  modality_interm <-
    eval(parse(text = paste("MiteMap$", modality, sep = "")))
  MiteMap$Modality_interm <- modality_interm

  if (!is.null(wrap)) {
    wrap_interm <-
      eval(parse(text = paste("MiteMap$", wrap, sep = "")))
    MiteMap$Wrap <- wrap_interm
  }
  p <- ggplot(
    MiteMap,
    aes(
      x = x.mm.,
      y = factor(Modality_interm),
      color = factor(Modality_interm)
    )
  ) +
    geom_violin() +
    ggtitle(paste("Position on x axis in function of", modality))

  if (!is.null(wrap)) {
    p <- p +
      facet_wrap(~Wrap)
  }
  return(p)
}
