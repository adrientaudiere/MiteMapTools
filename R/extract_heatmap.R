#' Create folder(s) with heatmaps extracted from zip files optionally organized
#' by a factor
#'
#' @param path_to_folder Path to folder
#' @param factor (character, default NULL) Set to a column name or a vector of
#'   column names to create subfolder for each levels of the modality.
#'   If multiple factors are provided, they value will be collapsed with "_".
#'   If left to NULL, all heatmaps are extracted in the same folder.
#' @param output_path (default =  "Heatmap") The path to the output folder.
#' @param verbose (Logical, default = TRUE) If TRUE, the function print additional
#'  information.
#' @param force Force overwriting the path to output_path
#' @param heatmap_file_name Name of the heatmap file in the zip file. Default is
#'  "_carte_thermique_" (sorry for french).
#' @param ... Other params for be passed on to [import_mitemap()]
#' @return Nothing. Create files in folders.
#' @export
#' @author Adrien Taudière
#' @examples
#' \dontrun{
#' extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   factor = "Treatment"
#' )
#' unlink("Heatmap", recursive = TRUE)
#' extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   factor = c("Treatment","Biomol_sp")
#' )
#' extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   factor = "Biomol_sp", force = TRUE
#' )
#' extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   clean = FALSE, force = TRUE
#' )
#' unlink("Heatmap", recursive = TRUE)
#' }
extract_heatmap <- function(path_to_folder,
                            factor = NULL,
                            output_path = "Heatmap",
                            verbose = TRUE,
                            force = FALSE,
                            heatmap_file_name = "_carte_thermique_",
                            ...) {
  if (force) {
    unlink(output_path, recursive = TRUE)
  }

  zip_files <- list.files(
    path_to_folder,
    pattern = ".zip",
    full.names = TRUE,
    include.dirs = TRUE
  )

  if (verbose) {
    cli::cli_alert_info("Extracting heatmaps from {length(zip_files)} zip file{?s}")
  }

  mm <- import_mitemap(path_to_folder, verbose = verbose, force = force, ...)
  dir.create(output_path)

  if (verbose) {
    cli::cli_progress_bar("Extracting heatmaps", total = length(zip_files))
  }

  for (i in 1:length(zip_files)) {
    if (verbose) {
      cli::cli_progress_update()
    }
    files_in_zip <- unzip(zip_files[i], list = TRUE)
    heatmap_file <-
      files_in_zip$Name[grepl(".png", files_in_zip$Name)]
    heatmap_name <- gsub(
      heatmap_file_name, "",
      gsub(".png", "", heatmap_file)
    )

    if (is.null(factor)) {
      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, sep = "")
      )
    } else {
      mod <- mm |>
        filter(File_name == heatmap_name) |>
        mutate(combined_factor = paste(!!!syms(factor), sep = "_")) |>
        pull(combined_factor) |>
        unique()

      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, "/", mod, sep = "")
      )
    }
  }

  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Heatmaps extracted to {.path {output_path}}")
  }
}
