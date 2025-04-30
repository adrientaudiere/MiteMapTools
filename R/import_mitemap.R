#' Import MiteMapTools data
#' @param path_to_folder Path to the folder with zip files containing csv and with one metadata files in the .xlsx format
#' @param path_to_metadata Path to the csv file (or xlsx if format_metadata="xlsx").
#'   If there is only one csv file set path_to_metadata="csv" (or "xlsx").
#'   Need to be set using a complete path to csv file if this file is not in
#'   the folder or if their is multiple xlsx/csv files in the folder.
#' @param format_metadata Either csv or xlsx
#' @param type_of_files (default : "raw_data") A pattern to select the type
#'   of csv (either "raw_data", "CH" or "HH") files inside the zip files.
#' @param delete_parenthesis (Logical, default FALSE) Do we delete parenthesis with a number inside in the name of the files. Note that the name of the csv inside a zip file with a parenthesis do not have parenthesis into this name. Thus the default value to TRUE is recommended either in delete_parenthesis or in replace_parenthesis
#' @param replace_parenthesis (Logical, default TRUE) Replace xxx(1) by xxx_1
#' @param delete_space (Logical, default TRUE)
#' @param messages (Logical, default TRUE) Do we print some warnings?
#' @param csv_with_correction (Logical, default FALSE) If TRUE an if present in
#'   the zip files, position files with correction (center and reduce to 0) are used.
#' @param remove_csv_folder  (Logical, default TRUE) If FALSE, the csv_folder is
#'   kept. May be useful for debugging.
#' @param dec decimal for the csv metadata files.
#' @param force Force overwriting the path to csv_folder
#' @param skip (default 1) skip the first line of the metadata files.
#' @param colnames_metadata Column names for metadata column. Note that File_name
#'   is required and must correspond to the names of zip files.
#'
#' @return A list of 4 elements
#' - `resulting_data`: a (possibly huge) tibble with metadata information and position in
#'   x, y and time.
#' - `files_not_in_csv`: a list of files not present in csv filenames from zip files.
#' - `files_not_in_metada`: a list of files not funded in metadata.
#' - `duplicate_file_name_in_metadata`: a list of duplicated file names in metadata.
#'
#'  By default, the `resulting_data` slot is structured in 12 columns (The fifth ones are from zip files
#'  and the other from metadata files).
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
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' mm_csv <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "POUL6", package = "MiteMapTools")
#' ))
#' dim(mm_csv$resulting_data)
#'
#' mm_HH <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "POUL6", package = "MiteMapTools"),
#'   type_of_files = "HH"
#' ))
#' dim(mm_HH$resulting_data)
#'
#' mm_xlsx <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "POUL6", package = "MiteMapTools"),
#'   format_metadata = "xlsx"
#' ))
#'
#' dim(mm_xlsx$resulting_data)
import_mitemap <- function(path_to_folder,
                           path_to_metadata = NULL,
                           format_metadata = "csv",
                           type_of_files = "raw_data",
                           delete_parenthesis = FALSE,
                           replace_parenthesis = TRUE,
                           delete_space = TRUE,
                           messages = TRUE,
                           csv_with_correction = FALSE,
                           remove_csv_folder = TRUE,
                           dec = ",",
                           force = FALSE,
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
                           )) {
  csv_folder_temp <- paste0(tempdir(), "/", "csv_folder")

  if (force) {
    unlink(csv_folder_temp, recursive = TRUE)
  }

  if (file.exists(csv_folder_temp)) {
    stop(
      "The folder", csv_folder_temp, "is present in your path workink directory.
      Please (i) manually delete it or (ii) call import_mite_map with force=TRUE or run
      (iii) unlink(paste0(tempdir(), '/', 'csv_folder'), recursive = TRUE)."
    )
  }

  zip_files <- list.files(
    path_to_folder,
    pattern = ".zip",
    full.names = T,
    include.dirs = T
  )

  if (length(zip_files) == 0) {
    unlink(csv_folder_temp, recursive = TRUE)

    stop(paste(
      "The folder",
      path_to_folder,
      "do not contain any zip files.",
      sep = " "
    ))
  }

  if (type_of_files == "raw_data") {
    type_of_files_in_french <- "donnees_brutes"
  } else if (type_of_files == "HH") {
    type_of_files_in_french <- "formeD"
  } else if (type_of_files == "CH") {
    type_of_files_in_french <- "formeC"
  }

  for (i in 1:length(zip_files)) {
    files_in_zip <- unzip(zip_files[i], list = TRUE)
    file_of_interest <-
      files_in_zip$Name[grepl(type_of_files_in_french, files_in_zip$Name)]

    if (csv_with_correction) {
      file_of_interest <-
        file_of_interest[grepl("s_cor.csv", file_of_interest)]
    }
    if (length(file_of_interest) > 1) {
      unlink(csv_folder_temp, recursive = TRUE)

      stop(paste(
        "There is multiple csv files for the format.",
        type_of_files,
        sep = ""
      ))
    }
    if (length(file_of_interest > 0)) {
      unzip(zip_files[i], file_of_interest, exdir = csv_folder_temp)
    }
  }

  file.remove(list.files(csv_folder_temp, pattern = "jpg", full.names = T)) # remove jpg files if present
  file.remove(list.files(csv_folder_temp, pattern = "png", full.names = T)) # remove png files if present
  csv_files <- list.files(csv_folder_temp, full.names = T)


  csv_list <- list()
  for (f in csv_files) {
    csv_list[[f]] <- read.delim(f)
  }
  df <- data.table::rbindlist(csv_list, idcol = "File_name")

  if (type_of_files == "CH") {
    df$File_name <- gsub(
      ".csv", "",
      gsub(
        "_Donnees_traitees_formeC_",
        "",
        gsub("csv_folder/", "", df$File_name)
      )
    )
  } else if (type_of_files == "HH") {
    df$File_name <- gsub(
      ".csv", "",
      gsub(
        "_Donnees_traitees_formeD_",
        "",
        gsub("csv_folder/", "", df$File_name)
      )
    )
  } else if (type_of_files == "raw_data") {
    df$File_name <- gsub(
      ".csv", "",
      gsub(
        "_donnees_brutes_",
        "",
        gsub("csv_folder/", "", df$File_name)
      )
    )
    if (csv_with_correction) {
      df$File_name <- gsub("_cor", "", df$File_name)
    }
  } else {
    stop("type_of_files must be one of 'CH', 'HH' or 'raw_data'")
  }

  df <- tibble(df) |>
    mutate(File_name = gsub(File_name, pattern = paste0(tempdir(), "/"), replacement = ""))

  if (is.null(path_to_metadata)) {
    if (format_metadata == "csv") {
      path_to_metadata <-
        list.files(path_to_folder,
          pattern = ".csv",
          full.names = T
        )
      metadata <- read.csv(path_to_metadata, dec = dec, skip = skip)
    } else if (format_metadata == "xlsx") {
      path_to_metadata <-
        list.files(path_to_folder,
          pattern = ".xlsx",
          full.names = T
        )
      metadata <- read_excel(path_to_metadata)
    }
  } else {
    if (length(path_to_metadata) > 1) {
      stop(
        "There is more than one xlsx/csv file in the folder.
        Use the path to metadata argument to set the correct path to the metadata file."
      )
    }
    metadata <- read.csv(path_to_metadata, dec = dec, skip = skip)
  }
  colnames(metadata) <- colnames_metadata

  if (delete_parenthesis) {
    metadata$File_name <-
      gsub("\\([[:xdigit:]]\\)", "", metadata$File_name, fixed = F)
  } else if (replace_parenthesis) {
    metadata$File_name <-
      sub("\\)", "", sub("\\(", "_", metadata$File_name))
  }

  if (delete_space) {
    metadata$File_name <- gsub(" ", "", metadata$File_name)
  }

  duplicated_file_name <-
    metadata$File_name[duplicated(metadata$File_name)]
  metadata <-
    metadata[!metadata$File_name %in% duplicated_file_name, ]

  df_final <- df |>
    inner_join(distinct(metadata, File_name, .keep_all = TRUE), by = c("File_name" = "File_name")) %>%
    tibble()

  if (nrow(df_final) == 0) {
    message("None correspondance between metadata and files names")
    return(NULL)
  }

  files_not_in_metadata <-
    unique(df$File_name)[!unique(df$File_name) %in% metadata$File_name]
  nb_files_not_in_metadata <- length(files_not_in_metadata)

  files_not_in_csv <-
    metadata$File_name[!metadata$File_name %in% df$File_name]
  nb_files_not_in_csv <- length(files_not_in_csv)

  if (ncol(df) + ncol(metadata) != ncol(df_final) + 1) {
    stop("The number of columns (variables) is not the addition of csv + metadata.")
  }

  if (messages) {
    if (nb_files_not_in_csv != 0) {
      warning(
        paste(
          nb_files_not_in_csv,
          "file(s) described in metadata were not present in the list of csv files"
        )
      )
    }

    if (nb_files_not_in_csv != 0) {
      warning(
        paste(
          nb_files_not_in_metadata,
          "file(s) present in the list of csv files were not described in metadata"
        )
      )
    }
    message(paste(
      "\n",
      "The final number of samples for folder is ",
      nrow(df_final),
      ".\n",
      sep = ""
    ))
  }

  res <- list()
  res$resulting_data <- df_final
  res$resulting_data$Date <-
    as.POSIXct(strptime(res$resulting_data$Date, "%d/%m/%Y"))

  res$files_not_in_csv <- files_not_in_csv
  res$files_not_in_metadata <- files_not_in_metadata
  res$duplicate_file_name_in_metadata <- duplicated_file_name

  if (remove_csv_folder) {
    unlink(csv_folder_temp, recursive = TRUE)
  }

  return(res)
}

#' Import mitemap from multiple folders
#'
#' @param folders A list of path
#' @param type_of_files A pattern to select the type of csv (either "raw_data", "CH" or "HH")
#' files using their name (e.g. "CH")
#' @param path_to_metadata A list of path
#' @param verbose (logical). If TRUE, print additional information.
#' @param ...  Other params for be passed on to [import_mitemap()]
#'
#' @return A list of 4 elements. See ?[import_mitemap()]
#'
#' @export
#' @author Adrien Taudière
#'
import_mitemap_from_multiple_folder <-
  function(folders = NULL,
           type_of_files = NULL,
           path_to_metadata = NULL,
           verbose = TRUE,
           ...) {
    res <- list()
    for (i in 1:length(folders)) {
      if (verbose) {
        print(basename(folders[i]))
      }
      res[[i]] <-
        import_mitemap(
          path_to_folder = folders[[i]],
          type_of_files = type_of_files,
          path_to_metadata = path_to_metadata[[i]],
          ...
        )
      if ("MiteMap.y" %in% names(res[[i]]$resulting_data)) {
        res[[i]]$resulting_data$MiteMap.y <-
          as.character(res[[i]]$resulting_data$MiteMap.y)
      }
      if ("MiteMap" %in% names(res[[i]]$resulting_data)) {
        res[[i]]$resulting_data$MiteMap <-
          as.character(res[[i]]$resulting_data$MiteMap)
      }
      if ("Bag" %in% names(res[[i]]$resulting_data)) {
        res[[i]]$resulting_data$Bag <-
          as.character(res[[i]]$resulting_data$Bag)
      }
    }

    final_res <- list()
    final_res$resulting_data <- res |>
      lapply(function(x) {
        mutate(x$resulting_data, across(everything(), as.character))
      }) |>
      bind_rows() |>
      readr::type_convert()

    final_res$files_not_in_csv <- list(lapply(res, function(x) {
      x$files_not_in_csv
    }))
    final_res$files_not_in_metadata <-
      list(lapply(res, function(x) {
        x$files_not_in_metadata
      }))
    final_res$duplicate_file_name_in_metadata <-
      list(lapply(res, function(x) {
        x$duplicate_file_name_in_metadata
      }))
    return(final_res)
  }
