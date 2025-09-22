#' Make violin plot of MiteMap data
#'
#' @param MiteMap (required) The result of import_mitemap ($resulting_data) for raw_data
#' @param factor : A name of column present in the MiteMap to separate violin plot.
#' @param wrap : A name of column present in the MiteMap to wrap violin plot.
#' @param prop_points (numeric between 0 and 1, default=NULL) If not NULL,
#'  a proportion of points is randomly sampled and added to the violin plot.
#'  This can help to visualize the data distribution, especially with large datasets.
#' @return A ggplot object
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' vioplot_mitemap(MM_data, "Treatment")
#' vioplot_mitemap(MM_data, factor = "Treatment", wrap = "Biomol_sp") +
#'   geom_boxplot(col="gray60", width=0.1) 
#'   
#' vioplot_mitemap(MM_data, "Treatment", prop_points=0.01)
vioplot_mitemap <- function(MiteMap,
                            factor = NULL,
                            wrap = NULL,
                            prop_points=NULL) {
  
  if(is.null(factor)){
    stop("You must provide a column name for factor.")
  }
  modality_interm <-
    eval(parse(text = paste("MiteMap$", factor, sep = "")))
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
    ggtitle(paste("Position on x axis in function of", factor))

  if (!is.null(wrap)) {
    p <- p +
      facet_wrap(~Wrap)
  }
  
  if(!is.null(prop_points)){
    sampled_data <- MiteMap |>
      group_by(Modality_interm) |>
      sample_frac(prop_points) |> 
      ungroup()
    
    p <- p + geom_jitter(data=sampled_data, alpha = 0.1) +
      labs("subtitle"=paste("with", round(nrow(sampled_data)), "points (", round(prop_points*100,2), "% of total data)"))
  }
  return(p)
}
