#' Filter (clean) MiteMap result
#' 
#' @description
#'  This function filter MiteMap data according to several parameters. It
#'  removes: the first seconds of each run, runs with aberrant range of x or y 
#'  values, points with aberrant x or y values, runs with a total time superior
#'  to maximum_time. It can also center the x and y values by additioning a 
#'  constant to all x and y values. By default the center of the arena is located
#'  at (0,0) with a diameter of 40mm.
#'
#' @param MiteMap (required) The result of import_mitemap ($resulting_data) for raw_data
#' @param first_seconds_to_delete (default: 2) How many seconds to delete?
#' @param bad_range_value_x (default: 45) The range value of x that is superior
#'  to what we expect. This param filter values on the File_name basis.
#' @param bad_range_value_y (default: 45) The range value of y that is superior
#'  to what we expect. This param filter values on the File_name basis.
#' @param max_x_value (default: 21) Maximum value for x axis.
#'   This param filter values on the time-point (one line) basis.
#' @param min_x_value (default: -21) Minimum value for x axis.
#'   This param filter values on the time-point (one line) basis.
#' @param max_y_value (default: 21) Maximum value for y axis.
#'   This param filter values on the time-point (one line) basis.
#' @param min_y_value (default: -21) Minimum value for y axis.
#'   This param filter values on the time-point (one line) basis.
#' @param maximum_time (default: 603) The final value of time (in seconds)).
#'   This param filter values on the File_name basis.
#' @param center_x (int, default 0) Center the value of x by additioning center_x mm
#'   to x.mm.
#' @param center_y (int, default 0) Center the value of y by additioning center_y mm
#'   to y.mm.
#' @param verbose (Logical, default = TRUE) If TRUE, the function print additional
#'  information.
#'
#' @return a filtered tibble
#' @export
#' @author Adrien Taudière
#'
#' @details
#'   The order of filtering is:
#'   1. Remove runs (filename) with a total time superior to 'maximum_time'.
#'   2. Remove the first 'first_seconds_to_delete' seconds of each run.
#'   3. Remove runs (filename) with aberrant range of x or y values 
#'     (i.e. superior to 'bad_range_value_x' or 'bad_range_value_y').
#'   4. Remove points with aberrant x or y values (i.e. x.mm. < 'min_x_value' or
#'   x.mm. > 'max_x_value', y.mm. < 'min_y value' or y.mm. > 'max_y_value').
#'   5. Center the x and y values by additioning 'center_x' and 'center_y' to all x and y values.
#' 
#' @examples
#' mm_csv <- import_mitemap(
#'   system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   file_name_column = "File (mite ID)", verbose = FALSE, clean=FALSE
#' )
#' dim(mm_csv)
#' 
#' MM_filtered_1 <- filter_mitemap(mm_csv)
#' dim(MM_filtered_1)
#' 
#' MM_filtered_2 <- filter_mitemap(mm_csv,
#'   bad_range_value_x = 41, 
#'   bad_range_value_y = 44,
#'   first_seconds_to_delete =1, 
#'   maximum_time = 301.1
#' )
#' dim(MM_filtered_2)
#' 
filter_mitemap <- function(MiteMap,
                           first_seconds_to_delete = 2,
                           bad_range_value_x = 42,
                           bad_range_value_y = 42,
                           max_x_value = 21,
                           min_x_value = -21,
                           max_y_value = 21,
                           min_y_value = -21,
                           maximum_time = 302,
                           center_x = 0,
                           center_y = 0,
                           verbose= TRUE) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }
  
  bad_time_value <-
    tapply(MiteMap$X..t.s., MiteMap$File_name, max) 
  bad_time_value <- bad_time_value[bad_time_value>maximum_time]
  
  new_MiteMap_interm <- MiteMap |>
    filter(X..t.s. >= first_seconds_to_delete) 

  range_x <- tapply(
    new_MiteMap_interm$x.mm.,
    new_MiteMap_interm$File_name,
    function(xx) {
      max(xx, na.rm = T) - min(xx, na.rm = T)
    }
  )
  bad_range_x <- range_x[range_x > bad_range_value_x]

  range_y <- tapply(
    new_MiteMap_interm$y.mm.,
    new_MiteMap_interm$File_name,
    function(xx) {
      max(xx, na.rm = T) - min(xx, na.rm = T)
    }
  )
  bad_range_y <- range_y[range_y > bad_range_value_y]

  new_MiteMap <- new_MiteMap_interm |>
    group_by(File_name) |>
    filter(!File_name %in% names(bad_range_x)) |>
    filter(!File_name %in% names(bad_range_y)) |>
    filter(!File_name %in% names(bad_time_value)) |>
    filter(x.mm. > min_x_value & x.mm. < max_x_value) |>
    filter(y.mm. > min_y_value & y.mm. < max_y_value) |>
    mutate(x.mm. = x.mm. + center_x, y.mm. = y.mm. + center_y)

if(verbose){
    message(
      paste0(
        "Rows removed when clearing for run with times sup to maximum_time: ",
        sum(MiteMap$File_name %in% names(bad_time_value)),
        " (", length(bad_time_value), " runs)\n"
      ),
      paste0(
        "Rows removed when clearing the first secondes: ",
        sum(MiteMap$X..t.s. < first_seconds_to_delete),
        "\n"
      ),
      paste0(
        "Rows removed when clearing bad x range: ",
        sum(MiteMap$File_name %in% names(bad_range_x)),
        " (", length(bad_range_x), " runs)\n"
      ),
      paste0(
        "Rows removed when clearing bad y range: ",
        sum(MiteMap$File_name %in% names(bad_range_y)),
        " (", length(bad_range_y), " runs)\n"
      ),
      paste0(
        "Rows removed when clearing bad x values: ",
        sum(MiteMap$x.mm. < min_x_value |
          MiteMap$x.mm. > max_x_value),
        "\n"
      ),
      paste0(
        "Rows removed when clearing bad y values: ",
        sum(MiteMap$y.mm. < min_y_value |
          MiteMap$y.mm. > max_y_value),
        "\n\n"
      ), 
      paste0(
        "Total rows after filtering: ",
        nrow(new_MiteMap),
        " (from ",
        nrow(MiteMap),
        ")\n"
      ), 
      paste0(
        "Total runs after filtering: ",
        length(unique(new_MiteMap$File_name)),
        " (from ",
        length(unique(MiteMap$File_name)),
        ")\n"
      )
    )
}
  return(new_MiteMap)
}
