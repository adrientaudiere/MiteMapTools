#' Summarize MiteMap data at the individual (File) level
#'
#' @details
#' The returns for the applied summary functions used. If you want custom
#' function such as quantile, median, you can use dplyr functions directly
#' inspired by this function (see examples).
#'
#'
#' @param MiteMap (required) The result of [import_mitemap()]
#' @param selected_cols (default = NULL) A character vector of column names
#' to include in the summary in addition to `File_name`.
#' If NULL, all columns are used.
#'
#' @returns A summary table (tibble) with one line per File_name and summary statistics.
#'
#' New columns are created with the suffixes corresponding to the applied summary function:
#'
#' - For numeric columns summary statistics include mean,
#'  standard deviation, minimum, and maximum values.
#' - For character and factor columns, the first unique value is retained.
#'  As file_name (individual mite) have unique metadata, all factor columns
#'   have one value by file_name.
#' - For logical columns, the proportion (mean) and number of TRUE values is
#'  calculated. The number of FALSE is also calculated.
#'  - A column `total_points` indicates the number of data points (rows)
#'   for each File_name.
#'
#'
#' @author Adrien Taudière
#' @export
#' @examples
#' mm_csv <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   file_name_column = "File (mite ID)"
#' ))
#'
#' sum_mm <- summarize_mitemap(mm_csv)
#' dim(sum_mm)
#' sum_mm_selected <- summarize_mitemap(mm_csv,
#'   selected_cols = c(
#'     "speed_mm_s",
#'     "distance_from_sources",
#'     "in_left_half_HH",
#'     "in_left_half_CH",
#'     "turning_angle",
#'     "turning_angle_odor",
#'     "turning_angle_ratio_odor",
#'     "Treatment"
#'   )
#' )
#'
#' ggplot(sum_mm_selected, aes(x = Treatment, y = distance_from_sources_mean, fill = Treatment)) +
#'   ggrain::geom_rain() +
#'   coord_flip()
#'
#' # Use custom summary with dplyr functions without the use of summarize_mitemap
#' mm_csv |>
#'   select("File_name", "Treatment", "Biomol_sp", "speed_mm_s") |>
#'   group_by(File_name) |>
#'   summarise(
#'     mean_speed = mean(speed_mm_s, na.rm = TRUE),
#'     across(where(is.character), ~ unique(.x)[1], .names = "{col}")
#'   ) |>
#'   ggplot(aes(x = Treatment, y = mean_speed, fill = Treatment)) +
#'   geom_col() +
#'   facet_wrap(~Biomol_sp)
#'
summarize_mitemap <- function(MiteMap,
                              selected_cols = NULL) {
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }

  if (!is.null(selected_cols)) {
    MiteMap <- select(MiteMap, all_of(c("File_name", selected_cols)))
  }

  summary_table <- MiteMap |>
    group_by(File_name) |>
    summarise(
      total_points = n(),
      across(where(is.numeric),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{col}_{fn}"
      ),
      across(where(is.character), ~ unique(.x)[1], .names = "{col}"),
      across(where(is.factor), ~ unique(.x)[1], .names = "{col}"),
      across(where(is.logical),
        list(
          prop_TRUE = ~ mean(.x, na.rm = TRUE),
          nb_TRUE = ~ sum(.x, na.rm = TRUE),
          nb_FALSE = ~ sum(!.x, na.rm = TRUE)
        ),
        .names = "{col}_{fn}"
      )
    ) |>
    ungroup()

  return(summary_table)
}
