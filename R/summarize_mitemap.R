#' Summarize MiteMap data at the individual (File) level
#'
#' @param MiteMap (required) The result of import_mitemap
#' @param num_cols A vector of names of numeric columns to summarize with mean, sd, min and max.
#' 
#' @returns A summary table (tibble) with one line per File_name and summary statistics.
#' 
#' New columns are created with the suffixes corresponding to the applied summary function:
#' 
#' - For selected numeric columns num_cols summary statistics include mean, 
#'  standard deviation, minimum, and maximum values.
#' - For character and factor columns, the first unique value is retained. 
#'  As file_name (individual mite) have unique metadata, all factor columns 
#'   have one value by file_name.
#' - For logical columns, the proportion (mean) and number of TRUE values is 
#'  calculated. The number of FALSE is also calculated.
#' - For other numeric columns, the mean value is calculated.
#' 
#' 
#' @author Adrien Taudière
#' @export
#' @import dplyr
#' @examples
#' mm_csv <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   file_name_column="File (mite ID)"
#' ))
#' sum_mm <- summarize_mitemap(mm_csv)
#' ggplot(sum_mm, aes(x=Treatment, y=distance_from_sources_mean, fill=Treatment)) +
#' ggrain::geom_rain() +
#' coord_flip()
summarize_mitemap <- function(MiteMap, num_cols = c("distance_from_previous", "speed_mm_s", "distance_from_sources", 
                                                    "in_left_half_HH", "in_left_half_CH", "turning_angle_clockwise", 
                                                    "turning_angle", "turning_angle_odor_clockwise", "turning_angle_odor", 
                                                    "turning_angle_ratio_odor")) {
  
  if (!is_tibble(MiteMap)) {
    MiteMap <- MiteMap$resulting_data
  }
  
  summary_table <- MiteMap |>
    group_by(File_name)|>
    dplyr::summarise(
      total_points = n(),
      across(any_of(num_cols), 
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE),
                  min = ~min(.x, na.rm = TRUE),
                  max = ~max(.x, na.rm = TRUE)
             ), .names = "{col}_{fn}"),
      across(where(is.character), ~ unique(.x)[1], .names = "{col}"),
      across(where(is.factor), ~ unique(.x)[1], .names = "{col}"),
      across(where(is.logical), 
             list(prop_TRUE = ~mean(.x, na.rm = TRUE),
                  nb_TRUE = ~sum(.x, na.rm = TRUE),
                  nb_FALSE = ~sum(!.x, na.rm = TRUE)), 
             .names = "{col}_{fn}"),
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{col}_mean")
    ) |>
    ungroup() 

  return(summary_table)
}