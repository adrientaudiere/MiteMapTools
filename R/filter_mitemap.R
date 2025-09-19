#' Filter MiteMap result
#'
#' @param MiteMap (required) The result of import_mitemap ($resulting_data) for raw_data
#' @param HH The result of [import_mitemap()] ($resulting_data) for type_of_files = "HH". It remove run with (i) time in <0, time out <0 and time_in/(time_in + time_out) > 1 in the HH files.
#' @param CH The result of [import_mitemap()] ($resulting_data) for type_of_files = "CH". It remove run with (i) time in <0, time out <0 and time_in/(time_in + time_out) > 1 in the CH files.
#' @param first_seconds_to_delete (default: 2) How many seconds we need to delete?
#' @param bad_range_value_x (default: 42) The range value of x that is superior to what we expect.
#'   This param filter values on the File_name basis.
#' @param bad_range_value_y (default: 42) The range value of y that is superior to what we expect.
#'   This param filter values on the File_name basis.
#' @param max_x_value (default: 42) Maximum value for x axis.
#'   This param filter values on the point (one line) basis.
#' @param min_x_value (default: 0) Minimum value for x axis.
#'   This param filter values on the point (one line) basis.
#' @param max_y_value (default: 42) Maximum value for y axis.
#'   This param filter values on the point (one line) basis.
#' @param min_y_value (default: 0) Minimum value for y axis.
#'   This param filter values on the point (one line) basis.
#' @param maximum_time (default: 603) Maximum time for one run (in seconds)).
#' @param center_x (int, default 0) Center the value of x by additioning center_x mm
#'   to x.mm.
#' @param center_y (int, default 0) Center the value of y by additioning center_y mm
#'   to y.mm.
#'
#' @return a filtered `resulting table` of class tibble
#' @export
#'
#' @examples
#' MM_filtered_centered <- filter_mitemap(MM_data,
#'   center_x = -20, center_y = -20,
#'   bad_range_value_x = 50, bad_range_value_y = 50
#' )
#' filter_mitemap(MM_data,
#'   HH = MM_ind_data$resulting_data,
#'   bad_range_value_x = 50, bad_range_value_y = 50
#' )
filter_mitemap <- function(MiteMap,
                           HH = NULL,
                           CH = NULL,
                           first_seconds_to_delete = 2,
                           bad_range_value_x = 50,
                           bad_range_value_y = 50,
                           max_x_value = 42,
                           min_x_value = 0,
                           max_y_value = 42,
                           min_y_value = 0,
                           maximum_time = 603,
                           center_x = 0,
                           center_y = 0) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }

  range_x <- tapply(
    MiteMap$x.mm.,
    MiteMap$File_name,
    function(xx) {
      max(xx, na.rm = T) - min(xx, na.rm = T)
    }
  )
  bad_range_x <- range_x[range_x > bad_range_value_x]

  range_y <- tapply(
    MiteMap$y.mm.,
    MiteMap$File_name,
    function(xx) {
      max(xx, na.rm = T) - min(xx, na.rm = T)
    }
  )
  bad_range_y <- range_y[range_y > bad_range_value_y]

  new_MiteMap1 <- MiteMap %>%
    group_by(File_name) %>%
    dplyr::filter(X..t.s. >= first_seconds_to_delete) %>%
    dplyr::filter(!File_name %in% names(bad_range_x)) %>%
    dplyr::filter(!File_name %in% names(bad_range_y)) %>%
    dplyr::filter(x.mm. > min_x_value & x.mm. < max_x_value) %>%
    dplyr::filter(y.mm. > min_y_value & y.mm. < max_y_value)

  max_times <-
    tapply(new_MiteMap1$X..t.s., new_MiteMap1$File_name, max)

  new_MiteMap2 <-
    new_MiteMap1[new_MiteMap1$File_name %in% names(max_times[max_times < maximum_time]), ]

  new_MiteMap2 <- new_MiteMap2 |>
    mutate(x.mm. = x.mm. + center_x, y.mm. = y.mm. + center_y)


  if (!is.null(HH)) {
    HH <-
      HH %>%
      mutate(prop_Tin = Tin.s. / (Tout.s. + Tin.s.)) %>%
      dplyr::filter(prop_Tin < 1) %>%
      dplyr::filter(Tout.s. > 0) %>%
      dplyr::filter(Tin.s. > 0)

    new_MiteMap3 <- new_MiteMap2 %>%
      dplyr::filter(File_name %in% HH$File_name)
  }

  if (!is.null(CH)) {
    CH <-
      CH %>%
      mutate(prop_Tin = Tin.s. / (Tout.s. + Tin.s.)) %>%
      dplyr::filter(prop_Tin < 1) %>%
      dplyr::filter(Tout.s. > 0) %>%
      dplyr::filter(Tin.s. > 0)

    if (!is.null(HH)) {
      new_MiteMap3 <- new_MiteMap2 %>%
        dplyr::filter(File_name %in% CH$File_name)
    } else {
      new_MiteMap3 <- new_MiteMap3 %>%
        dplyr::filter(File_name %in% CH$File_name)
    }
  }

  if (!is.null(HH) | !is.null(CH)) {
    message(
      paste(
        "Row removed when clearing the first secondes: ",
        sum(MiteMap$X..t.s. < first_seconds_to_delete),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x range: ",
        sum(MiteMap$File_name %in% names(bad_range_x)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y range: ",
        sum(MiteMap$File_name %in% names(bad_range_y)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x values: ",
        sum(MiteMap$x.mm. < min_x_value |
          MiteMap$x.mm. > max_x_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y values: ",
        sum(MiteMap$y.mm. < min_y_value |
          MiteMap$y.mm. > max_y_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for bad HH and/or CH: ",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap3)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for run with times sup to maximum_time",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap2)),
        "\n",
        sep = ""
      )
    )
    return(new_MiteMap3)
  } else {
    message(
      paste(
        "Row removed when clearing the first secondes: ",
        sum(MiteMap$X..t.s. < first_seconds_to_delete),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x range: ",
        sum(MiteMap$File_name %in% names(bad_range_x)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y range: ",
        sum(MiteMap$File_name %in% names(bad_range_y)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x values: ",
        sum(MiteMap$x.mm. < min_x_value |
          MiteMap$x.mm. > max_x_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y values: ",
        sum(MiteMap$y.mm. < min_y_value |
          MiteMap$y.mm. > max_y_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for run with times sup to maximum_time: ",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap2)),
        "\n",
        sep = ""
      )
    )
    return(new_MiteMap2)
  }
}
