#' Rename files with number inside a folder
#'
#' @param path_to_folder (required) Path to folder
#' @param remove_zip_files_with_parenthesis (logical, default TRUE). Do we
#'   remove zip_files with parenthesis in the name?
#'
#' @returns Nothing. Only modify files in a folder
#' @author Adrien Taudière
#' @export
#'
rename_files_with_number <- function(path_to_folder, remove_zip_files_with_parenthesis = TRUE) {
  old_wd <- getwd()
  folders_to_renamed <- list.files(here::here(path_to_folder),
    pattern = "\\([[:xdigit:]]\\)",
    include.dirs = TRUE,
    full.names = TRUE
  )

  for (folder in folders_to_renamed) {
    dir.create("unzip_temp")
    setwd("unzip_temp")
    zip::unzip(folder)
    names_files <- list.files()
    for (file in names_files) {
      ext <- get_file_extension(file)
      file.rename(file, paste0(
        sub(paste0(".", ext), "", file),
        "_",
        sub(").zip", "", sub(".*\\(", "", folder, perl = TRUE)),
        paste0(".", ext)
      ))
    }

    zip::zip(sub("\\)", "", sub(" \\(", "_", folder)), files = list.files())
    setwd(old_wd)
    unlink("unzip_temp", recursive = TRUE, force = TRUE)
    if (remove_zip_files_with_parenthesis) {
      lapply(folders_to_renamed, unlink)
    }
  }
  setwd(old_wd)
  unlink("unzip_temp", recursive = TRUE, force = TRUE)
}


#' Get the extension of a file
#'
#'
#' Internally used in [rename_files_with_number()] for ex.
#' Warning: don't work when there is '.' in the name of the file before the extension
#' @param file_path (required): path to a file
#' @author Adrien Taudière
#'
#' @return The extension of a file.
#' @export
#' @examples
#' get_file_extension("my_file.csv")
#' get_file_extension("my.file.csv")
#' get_file_extension("my_file.csv.zip")
get_file_extension <- function(file_path) {
  if (stringr::str_count(file_path, "\\.") == 0) {
    stop("There is no '.' inside your file path: ", file_path)
  }
  if (stringr::str_count(file_path, "\\.") > 1) {
    warning("There is more than one '.' inside your file path: ", file_path)
  }
  file_ext <- strsplit(basename(file_path), ".", fixed = TRUE)[[1]][-1]
  return(file_ext)
}


#' Plot individual map
#'
#' @param MiteMap MiteMap (required) The result of import_mitemap
#'   ($resulting_data) for raw_data
#' @param ind_index (required, vector of int, default = c(1))
#'   The index numbers of the file(s) (individual(s)) you want to plot
#' @param time_animation (logical, default FALSE) Does the plot is animate
#'   using gganimate ?
#'
#' @param add_base_circle (logical, default FALSE) Does the base circle is plot ?
#' @param center_base_circle (int) The center of the base circle. Only used if
#'   `center_base_circle` is TRUE.
#' @param breaks_animation (int) The number of breaks in animation. Only used if
#'   `time_animation` is TRUE.
#' @param diameter_base_circle (int) The diameter of the base circle.
#'    Only used if `center_base_circle` is TRUE.
#' @param npoints_base_circle (int) The number of point to draw the base circle.
#'    Only used if `center_base_circle` is TRUE.
#' @param linewidth (int) The linewidth of the path
#' @param alpha_shadow (int [0,1]) Alpha value (transparency) for shadow path.
#'    Only used if time_animation` is TRUE.
#'
#' @returns A list of ggplot object
#' @author Adrien Taudière
#' @export
#'
#' @examples
#' MM <- filter_mitemap(MM_example,
#'   center_x = -28, center_y = -22, max_x_value = 52, max_y_value = 52
#' )
#'
#' p_l <- plot_ind_mitemap(MM, ind_index = c(1, 2))
#' p_l[[1]] + p_l[[2]] & theme(legend.position = "none")
#'
#' p_l_base_c <- plot_ind_mitemap(MM, add_base_circle = TRUE, linewidth = 1.7)
#' p_l_base_c[[1]] + scale_color_gradient(name = "Speed", trans = "log10", low = "cyan", high = "red")
#'
#' p_l_anim <- plot_ind_mitemap(MM, time_animation = TRUE, breaks_animation = 15)
#' p_l_anim[[1]]
#'
#' p_l <- plot_ind_mitemap(MM, ind_index = c(1:6))
#' (p_l[[1]] + p_l[[2]] + p_l[[3]]) /
#'   (p_l[[4]] + p_l[[5]] + p_l[[6]]) +
#'   plot_layout(guides = "collect") & scale_color_gradient(
#'   name = "Speed", trans = "log1p", low = "cyan", high = "red",
#'   limits = c(
#'     min(unlist(lapply(p_l, function(x) {
#'       min(x$data$dist_from_i_minus_1, na.rm = T)
#'     }))),
#'     max(unlist(lapply(p_l, function(x) {
#'       max(x$data$dist_from_i_minus_1, na.rm = T)
#'     })))
#'   )
#' )
#'
#' (p_l[[1]] + p_l[[2]] + p_l[[3]]) /
#'   (p_l[[4]] + p_l[[5]] + p_l[[6]]) +
#'   plot_layout(guides = "collect") & scale_color_viridis_c(name = "Speed", trans = "log1p", limits = c(0, 2))
plot_ind_mitemap <- function(MiteMap,
                             ind_index = c(1),
                             time_animation = FALSE,
                             add_base_circle = FALSE,
                             center_base_circle = c(0, 0),
                             breaks_animation = 10,
                             diameter_base_circle = 40,
                             npoints_base_circle = 100,
                             linewidth = 1.2,
                             alpha_shadow = 0.4) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }

  p <- list()
  for (i in ind_index) {
    p[[i]] <- ungroup(MiteMap) |>
      filter(File_name == unique(File_name)[[i]]) |>
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

      p[[i]] <- p[[i]] + geom_path(data = dat_circle, aes(x, y))
    }
    if (time_animation) {
      p[[i]] <- p[[i]] +
        gganimate::transition_reveal(time) +
        gganimate::shadow_mark(alpha = alpha_shadow)
    }
  }
  return(p)
}
