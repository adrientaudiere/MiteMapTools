#' example dataset from MiteMapTools
#'
#' Obtained using code :
#'
#'  MM_data <- suppressWarnings(import_mitemap(system.file("extdata", "POUL6", package = "MiteMapTools")))
#'
#'  save(MM_data, file="data/MM_data.rda")
#'
#' @format
#' A list of 4 elements
#' - `resulting_data`: a (possibly huge) tibble with metadata information and position in
#'   x, y and time.
#' - `files_not_in_csv`: a list of files not present in csv filenames from zip files.
#' - `files_not_in_metada`: a list of files not funded in metadata.
#' - `duplicate_file_name_in_metadata`: a list of duplicated file names in metadata.
#'
#' The `resulting_data` slot is structured in 12 columns (The fifth ones are from zip files
#'  and the other from metadata files). See [import_mitemap()].
#'   - File_name
#'   - The time in second (position is recorded every 0.2s)
#'   - The position in x (in mm)
#'   - The position in y (in mm)
#'   - Boolean variable indicating if the individual has remained immobile since the last record (1 if immobile)
#'   - Run number
#'   - Date
#'   - Start time
#'   - Farm
#'   - MiteMap number
#'   - Bag
#'   - Modality
#'
#' @source <https://onlinelibrary.wiley.com/doi/10.1002/jez.2651>
#' @usage data(MM_data)
"MM_data"

#' example dataset from MiteMapTools for shape Half-half
#'
#' Obtained using code :
#' #TODO  `MM_ind_data <- suppressWarnings(import_mitemap(system.file("extdata", "POUL6", package = "MiteMapTools"), type_of_files = "HH"))`
#' @format
#' A list of 4 elements
#' - `resulting_data`: a (possibly huge) tibble with metadata information and position in
#'   x, y and time.
#' - `files_not_in_csv`: a list of files not present in csv filenames from zip files.
#' - `files_not_in_metada`: a list of files not funded in metadata.
#' - `duplicate_file_name_in_metadata`: a list of duplicated file names in metadata.
#'
#' The `resulting_data` slot is structured in 16 columns (The fifth ones are from zip files
#'  and the other from metadata files). See [import_mitemap()].
#'   - File_name
#'   - The Date and hour
#'   - The name of the MiteMap (can be redundant with MiteMap_number)
#'   - Total time spent in the half containing the odor source (second)
#'   - Total time spent in the opposite half (second)
#'   - Time spent immobile in the half containing the odor source (second)
#'   - Time spent immobile in the opposite half (second)
#'   - Total distance traveled in the half containing the odor source (mm)
#'   - Total distance traveled in the opposite half (mm)
#'   - Boolean variable indicating if the individual has remained immobile since the last record (1 if immobile)
#'   - Run number
#'   - Date
#'   - Start time
#'   - Farm
#'   - MiteMap number
#'   - Bag
#'   - Modality
#'
#' @source <https://onlinelibrary.wiley.com/doi/10.1002/jez.2651>
#' @usage data(MM_ind_data)
"MM_ind_data"
