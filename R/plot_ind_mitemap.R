#' Plot individual map
#'
#' @param MiteMap MiteMap (required) The result of import_mitemap
#'   ($resulting_data) for raw_data
#' @param file_names (vector of string, default = NULL)
#'  The names of the file(s) (individual(s)) you want to plot
#'  (e.g. c("MM012022_05_17_13h25m12s", "MM012022_05_17_09h23m48s")).
#'  If not NULL, ind_index is ignored.
#' @param ind_index (vector of int, default = c(1))
#'   The index numbers of the file(s) (individual(s)) you want to plot
#' @param time_animation (logical, default FALSE) Does the plot is animate
#'   using gganimate ?#'
#' @param add_base_circle (logical, default FALSE) Does the base circle is plot ?
#' @param add_odor_source (logical, default TRUE) Does the odor source is plot ?
#' @param label_odor_source (string, default = NULL) Label of the odor source.
#'   Only used if `add_odor_source` is TRUE. If NULL, no label is added.
#' @param center_base_circle (int) The center of the base circle. Only used if
#'   `center_base_circle` is TRUE.
#' @param breaks_animation (int) The number of breaks in animation. Only used if
#'   `time_animation` is TRUE.
#' @param diameter_base_circle (int) The diameter of the base circle.
#'    Only used if `center_base_circle` is TRUE.
#' @param npoints_base_circle (int) The number of point to draw the base circle.
#'    Only used if `center_base_circle` is TRUE.
#' @param linewidth (int) The linewidth of the path
#' @param alpha_shadow (int \[0,1\]) Alpha value (transparency) for shadow path.
#'    Only used if time_animation` is TRUE.
#'
#' @returns A list of ggplot object
#' @author Adrien Taudière
#' @export
#'
#' @examples
#' MM <- import_mitemap(
#'   system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   file_name_column = "File (mite ID)",
#'   verbose = FALSE
#' )
#'
#' library(patchwork)
#' p <- plot_ind_mitemap(MM, file_names = c("MM012022_05_17_13h22m59s", "MM012022_05_17_08h23m05s"))
#' p[[1]] + p[[2]] & theme(legend.position = "none")
#'
#'
#' p_l <- plot_ind_mitemap(MM, ind_index = c(1, 2))
#' p_l[[1]] + p_l[[2]] & theme(legend.position = "none")
#'
#' p_l_base_c <- plot_ind_mitemap(MM,
#'   add_base_circle = TRUE, linewidth = 1.7,
#'   label_odor_source = "Odor source"
#' )
#' p_l_base_c[[1]] + scale_color_gradient(name = "Speed", trans = "log10", low = "cyan", high = "red")
#'
#' if (!requireNamespace("gganimate", quietly = TRUE)) {
#'   p_l_anim <- plot_ind_mitemap(MM, time_animation = TRUE, breaks_animation = 15)
#'   p_l_anim[[1]]
#'
#'   p_l <- plot_ind_mitemap(MM, ind_index = c(1:6))
#'   (p_l[[1]] + p_l[[2]] + p_l[[3]]) /
#'     (p_l[[4]] + p_l[[5]] + p_l[[6]]) +
#'     plot_layout(guides = "collect") & scale_color_gradient(
#'     name = "Speed", trans = "log1p", low = "cyan", high = "red",
#'     limits = c(
#'       min(unlist(lapply(p_l, function(x) {
#'         min(x$data$dist_from_i_minus_1, na.rm = T)
#'       }))),
#'       max(unlist(lapply(p_l, function(x) {
#'         max(x$data$dist_from_i_minus_1, na.rm = T)
#'       })))
#'     )
#'   )
#'
#'   (p_l[[1]] + p_l[[2]] + p_l[[3]]) /
#'     (p_l[[4]] + p_l[[5]] + p_l[[6]]) +
#'     plot_layout(guides = "collect") &
#'     scale_color_viridis_c(name = "Speed", trans = "log1p", limits = c(0, 2))
#' }
plot_ind_mitemap <- function(MiteMap,
                             file_names = NULL,
                             ind_index = c(1),
                             time_animation = FALSE,
                             add_base_circle = FALSE,
                             add_odor_source = TRUE,
                             label_odor_source = NULL,
                             center_base_circle = c(0, 0),
                             breaks_animation = 10,
                             diameter_base_circle = 40,
                             npoints_base_circle = 100,
                             linewidth = 1.2,
                             alpha_shadow = 0.4) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }

  if (!is.null(file_names)) {
    ind_index <- which(unique(MiteMap$File_name) %in% file_names)
  }

  p <- list()
  for (fi_na in unique(MiteMap$File_name)[ind_index]) {
    p[[fi_na]] <- ungroup(MiteMap) |>
      filter(File_name == fi_na) |>
      mutate(time = rank(cut(X..t.s., breaks = breaks_animation))) |>
      mutate(x_change = lag(x.mm.) - x.mm.) |>
      mutate(y_change = lag(y.mm.) - y.mm.) |>
      mutate(dist_from_i_minus_1 = sqrt(x_change^2 + y_change^2)) |>
      ggplot(aes(x = x.mm., y = y.mm.)) +
      geom_path(aes(colour = dist_from_i_minus_1), linewidth = linewidth) +
      scale_color_viridis_c() +
      theme_minimal()

    if (add_base_circle) {
      circleFun <- function(center = c(0, 0),
                            diameter = 1,
                            npoints = 100) {
        r <- diameter / 2
        tt <- seq(0, 2 * pi, length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
      }

      dat_circle <- circleFun(
        center = center_base_circle,
        diameter = diameter_base_circle,
        npoints = npoints_base_circle
      )
      dat_circle$time_min <- 0

      p[[fi_na]] <- p[[fi_na]] + geom_path(data = dat_circle, aes(x, y))
    }
    if (add_odor_source) {
      p[[fi_na]] <- p[[fi_na]] +
        annotate("point",
          x = -19.5,
          y = 0,
          size = 7,
          shape = 8,
          color = "red"
        )
      if (!is.null(label_odor_source)) {
        p[[fi_na]] <- p[[fi_na]] +
          annotate("text", x = -17, y = 2, label = label_odor_source, color = "red")
      }
    }
    if (time_animation) {
      p[[fi_na]] <- p[[fi_na]] +
        gganimate::transition_reveal(time) +
        gganimate::shadow_mark(alpha = alpha_shadow)
    }
  }
  return(p)
}
