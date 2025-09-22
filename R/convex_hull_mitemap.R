#' Map convex hull on MiteMap
#'
#' @param MiteMap (required) The result of import_mitemap
#' @param unity (default = 1): size of square grid in mm
#' @param tbe (default = 3)
#' @param plot_show (Logical, default = TRUE) : Do we plot all mitemap ?
#' @param probs_quantile (default = 0.68) 
#' @param min_nb_spatial_points : Minimum number of spatial point to keep the sample
#' @param each_point_count_one (Logical ; default = FALSE) If TRUE, each spatial point
#'   count only for one time step instead. By default, if the mite stay 3t at a given
#'   spatial location, the spatial point count 3 times in the construction of the convex
#'   hull.
#' @param hull_col color of hull area
#' @param plot_center_of_mass (Logical, default = TRUE): Do we plot the center of mass?
#' @param verbose (Logical, default = TRUE) If TRUE, the function print additional
#' @return A dataframe with convex hull information for each run (File_name)
#'    and plot of convex hull for each run if plot_show = TRUE. 
#' @export
#' @author Adrien Taudière
#' @examples
#' ch <- convex_hull_mitemap(MM_data)
#' ch <- convex_hull_mitemap(MM_data, plot_show = FALSE)
#' full_join(ch, MM_data) |>
#'   ggplot(aes(x = Treatment, y = hull_area)) +
#'   geom_boxplot()
#'
#' full_join(ch, MM_data) |>
#'   group_by(File_name) |>
#'   summarise(
#'     hull_center_away_from_odor = sum(center_of_mass_x > 0) > 0,
#'     Treatment = unique(Treatment)
#'   ) |>
#'   ggplot(aes(fill = hull_center_away_from_odor, x = Treatment)) +
#'   geom_bar()
convex_hull_mitemap <- function(MiteMap,
                                unity = 1,
                                tbe = 3,
                                plot_show = TRUE,
                                probs_quantile = 0.68,
                                min_nb_spatial_points = 2,
                                each_point_count_one = FALSE,
                                hull_col = "red",
                                plot_center_of_mass = TRUE,
                                verbose = TRUE) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }

  hull_area <- list()
  center_of_mass <- list()
  center_of_area_x <- list()
  center_of_area_y <- list()
  hull_length <- list()

  for (run in unique(MiteMap$File_name)) {
    x <-
      floor(as.numeric(MiteMap[MiteMap$File_name == run, ]$x.mm.) * unity) /
        unity
    y <-
      floor(as.numeric(MiteMap[MiteMap$File_name == run, ]$y.mm.) * unity) /
        unity
    tbe_table <- table(paste(x, y, sep = ":"))
    if (length(tbe_table) < min_nb_spatial_points) {
     if(verbose){ 
       message(
        paste(
          "No convex Hull found for sample:",
          run,
          "(not enough spatial points)"
        )
      )}
      next
     }
    if(verbose){ 
    message(
      paste(
        "Number of spatial points conserved using tbe = ",
        tbe,
        ": ",
        sum(tbe_table > tbe),
        " (",
        round(sum(tbe_table > tbe) / length(tbe_table) * 100, 2),
        " %)",
        sep = ""
      )
    )}
    nms <- names(tbe_table)
    coords <-
      matrix(as.numeric(unlist(strsplit(nms, ":"))), ncol = 2, byrow = TRUE)
    if (each_point_count_one) {
      xy <- coords[tbe_table > tbe, ]
    } else {
      nms2 <- nms[tbe_table > tbe]
      couple <- paste(x, y, sep = ":")
      couple <- couple[couple %in% nms2]
      xy <-
        matrix(as.numeric(unlist(strsplit(couple, ":"))),
          ncol = 2,
          byrow = TRUE
        )
    }

    dst <- rowSums(scale(xy, scale = FALSE)^2)
    idx <- which(dst < quantile(dst, probs = probs_quantile))
    if (length(idx) == 0) {
      if(verbose) {
        message(
        paste(
          "No convex Hull found for sample:",
          run,
          "(none distance < quantile) "
        )
      )}
      next
    }
    idx0 <- chull(xy[idx, 1], xy[idx, 2])
    idx0 <- c(idx0, idx0[1])

    center_of_area_x[[run]] <- mean(xy[, 1])
    center_of_area_y[[run]] <- mean(xy[, 2])


    if (plot_show) {
      plot(
        coords,
        cex = tbe_table / 10,
        col = rgb(0, 0, 0, 0.4),
        xlim = c(-20, 20),
        ylim = c(-20, 20),
        asp = 1
      )
      plotrix::draw.circle(0, 0, 20, col = rgb(0, 0, 0.8, 0.2))
      points(center_of_area_x[[run]],
        center_of_area_y[[run]],
        pch = 19,
        col = hull_col
      )
      lines(xy[idx[idx0], 1], xy[idx[idx0], 2], lwd = 2, col = hull_col)

      title(run)
    }

    z <- xy[idx[idx0], ]
    hull_area[[run]] <- pracma::polyarea(z[, 1], z[, 2])
    if (nrow(z) > 2) {
      center_of_mass[[run]] <- pracma::poly_center(z[, 1], z[, 2])
    } else {
      center_of_mass[[run]] <- c(NA, NA)
    }


    hull_length[[run]] <- pracma::poly_length(z[, 1], z[, 2])
    if (plot_show) {
      if (plot_center_of_mass) {
        points(
          center_of_mass[[run]][1],
          center_of_mass[[run]][2],
          pch = 3,
          col = "black",
          cex = 2
        )
      }
    }
  }

  return(
    data.frame(
      "File_name" = names(unlist(hull_area)),
      "hull_area" = unlist(hull_area),
      "center_of_area_x" = unlist(center_of_area_x),
      "center_of_area_y" = unlist(center_of_area_y),
      "center_of_mass_x" = unlist(center_of_mass)[seq(1, 2 *
        length(center_of_mass), by = 2)],
      "center_of_mass_y" = unlist(center_of_mass)[seq(2, 2 *
        length(center_of_mass), by = 2)],
      "hull_length" = unlist(hull_length),
      row.names = names(unlist(hull_area))
    )
  )
}
