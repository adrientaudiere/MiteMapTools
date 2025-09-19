#' Create folder(s) with heatmap
#'
#' @param path_to_folder Path to folder
#' @param output_path (default =  "Heatmap")
#' @param path_to_metadata Path to metadata
#' @param format_metadata (either csv of xlsx)
#' @param modality (character, default "Modality") Set to a column names
#'  to create subfolder for each levels of the modality
#' @param subfolder_by_farm (Logical, default = TRUE) create subfolder for
#'   each farm
#' @param subfolder_by_modality (Logical, default = TRUE) create subfolder for
#'   each modality set in parameter `modality`
#' @param dec decimal for the csv metadata files.
#' @param skip (default 1) skip the first line of the metadata files.
#' @param colnames_metadata Column names for metadata column. Note that File_name
#'   is required and must correspond to the names of zip files.
#' @param force Force overwriting the path to output_path
#' @return Nothing. Create files in folders.
#' @export
#' @author Adrien Taudière
#' @examples
#' extract_heatmap(system.file("extdata", "POUL6", package = "MiteMapTools"))
#' unlink("Heatmap", recursive = TRUE)
extract_heatmap <- function(path_to_folder,
                         output_path = "Heatmap",
                         path_to_metadata = NULL,
                         format_metadata = "csv",
                         modality = "Modality",
                         subfolder_by_farm = TRUE,
                         subfolder_by_modality = TRUE,
                         dec = ",",
                         skip = 1,
                         colnames_metadata = c(
                           "Run_number",
                           "File_name",
                           "Date",
                           "Start_time",
                           "Farm",
                           "MiteMap_number",
                           "Bag",
                           "Modality"
                         ),
                         force = FALSE) {
  if (force) {
    unlink(output_path, recursive = TRUE)
  }

  zip_files <- list.files(
    path_to_folder,
    pattern = ".zip",
    full.names = T,
    include.dirs = T
  )

  if (is.null(path_to_metadata)) {
    if (format_metadata == "csv") {
      path_to_metadata <-
        list.files(path_to_folder,
          pattern = ".csv",
          full.names = T
        )
      if (length(path_to_metadata) > 1) {
        metadata <- list()
        for (i in 1:length(path_to_metadata)) {
          metadata[[i]] <- read.csv(path_to_metadata[[i]], dec = dec, skip = skip)
        }
        metadata <- metadata |>
          lapply(function(x) {
            mutate(x, across(everything(), as.character))
          }) |>
          bind_rows() |>
          readr::type_convert()
      } else {
        metadata <- read.csv(path_to_metadata, dec = dec, skip = skip)
      }
    } else if (format_metadata == "xlsx") {
      path_to_metadata <-
        list.files(path_to_folder,
          pattern = ".xlsx",
          full.names = T
        )
      if (length(path_to_metadata) > 1) {
        metadata <- list()
        for (i in 1:length(path_to_metadata)) {
          metadata[[i]] <- read_excel(path_to_metadata[[i]])
        }
        metadata <- metadata |>
          lapply(function(x) {
            mutate(x, across(everything(), as.character))
          }) |>
          bind_rows() |>
          readr::type_convert()
      } else {
        metadata <- read_excel(path_to_metadata)
      }
    }
  } else {
    metadata <- read.csv(path_to_metadata, dec = dec, skip = skip)
  }

  colnames(metadata) <- colnames_metadata
  dir.create(output_path)

  for (i in 1:length(zip_files)) {
    files_in_zip <- unzip(zip_files[i], list = TRUE)
    heatmap_file <-
      files_in_zip$Name[grepl(".png", files_in_zip$Name)]
    heatmap_name <- gsub(
      "_carte_thermique_", "",
      gsub(".png", "", heatmap_file)
    )

    mod <- na.omit(metadata[match(heatmap_name, metadata$File_name), modality])[1]
    farm <- na.omit(metadata[match(heatmap_name, metadata$File_name), "Farm"])[1]

    if (subfolder_by_modality & !subfolder_by_farm) {
      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, "/", mod, sep = "")
      )
    }

    if (!subfolder_by_modality & subfolder_by_farm) {
      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, "/", farm, sep = "")
      )
    }

    if (!subfolder_by_modality & !subfolder_by_farm) {
      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, sep = "")
      )
    }

    if (subfolder_by_modality & subfolder_by_farm) {
      unzip(zip_files[i],
        heatmap_file,
        exdir = paste(output_path, "/", farm, "/", mod, sep = "")
      )
    }
  }
}

#' Create folders with heatmap from multiples folders
#'
#' @param folders A list of path
#' @param ... Other params for be passed on to [extract_heatmap()]
#'
#' @return Nothing. Create files in folders.
#' @export
#' @author Adrien Taudière
extract_heatmap_from_multiple_folders <- function(folders, ...) {
  if (length(folders) == 1) {
    extract_heatmap(folders, output_path = gsub(".*/|$", "", gsub("/$", "", folders)), ...)
  } else {
    for (i in 1:length(folders)) {
      extract_heatmap(folders[i], output_path = gsub(".*/|$", "", folders[i]), ...)
    }
  }
}
