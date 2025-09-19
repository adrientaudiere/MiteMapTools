#' Import MiteMapTools data
#' @param path_to_folder Path to the folder with zip files containing csv and with one metadata files in the .xlsx format
#' @param path_to_metadata Path to the csv file (or xlsx if format_metadata="xlsx").
#'   If there is only one csv file set path_to_metadata="csv" (or "xlsx").
#'   Need to be set using a complete path to csv file if this file is not in
#'   the folder or if their is multiple xlsx/csv files in the folder.
#' @param format_metadata Either csv or xlsx
#' @param delete_parenthesis (Logical, default FALSE) Do we delete parenthesis with a number
#'   inside in the name of the files. Note that the name of the csv inside a zip
#'   file with a parenthesis do not have parenthesis into this name. Thus, we recommended to
#'   set TRUE at least in one of delete_parenthesis or replace_parenthesis parameter.
#' @param replace_parenthesis (Logical, default TRUE) Replace abc_name(1) by abc_name_1
#' @param delete_space (Logical, default TRUE) Delete_space in the file name. 
#' @param messages (Logical, default TRUE) Do we print some warnings?
#' @param csv_with_correction (Logical, default FALSE) If TRUE an if present in
#'   the zip files, position files with correction (center and reduce to 0) are used.
#' @param remove_csv_folder  (Logical, default TRUE) If FALSE, the csv_folder is
#'   kept. May be useful for debugging.
#' @param dec decimal for the csv metadata files.
#' @param force Force overwriting the path to csv_folder
#' @param return_with_logs (Locical, default FALSE). If TRUE, the returning 
#'   object is a list of 4 elements containing usefull information to explore
#'   unmatching name between file_names and metadata
#' @param center_x (int, default 0) Center the value of x by additioning center_x mm
#'   to x.mm.
#' @param center_y (int, default 0) Center the value of y by additioning center_y mm
#'   to y.mm.
#' @param compute_metrics (Logical, default TRUE). Are metrics such as time_immobile, 
#'   speed and turning angles are computed for each time step ? 
#' @param file_name_column Name for the column corresponding to the File_name.
#'
#' @return If `return_with_logs` is FALSE (default), the return object is only 
#'   the tibble called `resulting_data`. If `return_with_logs` is TRUE, it 
#'   return a list of 4 elements:
#' - `resulting_data`: a (possibly huge) tibble with metadata information and position in
#'   x, y and time.
#' - `files_not_in_csv`: a list of files not present in csv filenames from zip files.
#' - `files_not_in_metada`: a list of files not funded in metadata.
#' - `duplicate_file_name_in_metadata`: a list of duplicated file names in metadata.
#'
#'  By default, the `resulting_data` slot is structured in 4 obligated columns + columns from
#'   the metadata file + the computed metrics if compute_metrics is TRUE. 
#'   - File_name
#'   - X..t.s. - The time in second (position is recorded every 0.2s)
#'   - x.mm. - The position in x (in mm)
#'   - y.mm. - The position in y (in mm)
#'   - Metadata columns
#'   - Computed metrics
#' @export
#' @author Adrien Taudière
#'
#' @examples
#' mm_csv <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "POUL6", package = "MiteMapTools")
#' ))
#' dim(mm_csv$resulting_data)
#'
#' mm_xlsx <- suppressWarnings(import_mitemap(
#'   system.file("extdata", "POUL6", package = "MiteMapTools"),
#'   format_metadata = "xlsx"
#' ))
#'
#' dim(mm_xlsx$resulting_data)
#' 
#' # mm_xlsx$Date <-
#' #   as.POSIXct(strptime(mm_xlsx$Date, "%d/%m/%Y"))
import_mitemap <- function(path_to_folder,
                           path_to_metadata = NULL,
                           format_metadata = "csv",
                           delete_parenthesis = FALSE,
                           replace_parenthesis = TRUE,
                           delete_space = TRUE,
                           messages = TRUE,
                           csv_with_correction = FALSE,
                           remove_csv_folder = TRUE,
                           dec = ",",
                           force = FALSE,
                           return_with_logs = FALSE,
                           compute_metrics = TRUE,
                           file_name_column = "File_name") {
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
    full.names = TRUE,
    include.dirs = TRUE
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
 
  for (i in 1:length(zip_files)) {
    files_in_zip <- unzip(zip_files[i], list = TRUE)
    file_of_interest <-
      files_in_zip$Name[grepl("donnees_brutes", files_in_zip$Name)]

    if (csv_with_correction) {
      file_of_interest <-
        file_of_interest[grepl("s_cor.csv", file_of_interest)]
    }
    if (length(file_of_interest) > 1) {
      unlink(csv_folder_temp, recursive = TRUE)

      stop(paste(
        "There is multiple csv files for the format",
      ))
    }
    if (length(file_of_interest > 0)) {
      unzip(zip_files[i], file_of_interest, exdir = csv_folder_temp)
    }
  }

  file.remove(list.files(csv_folder_temp, pattern = "jpg", full.names = TRUE)) # remove jpg files if present
  file.remove(list.files(csv_folder_temp, pattern = "png", full.names = TRUE)) # remove png files if present
  csv_files <- list.files(csv_folder_temp, full.names = TRUE)

  csv_list <- list()
  for (f in csv_files) {
    csv_list[[f]] <- read.delim(f)
  }
  df <- data.table::rbindlist(csv_list, idcol = File_name) 

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

  df <- tibble(df) |>
    mutate(File_name = gsub(File_name, pattern = paste0(tempdir(), "/"), replacement = ""))

  if (is.null(path_to_metadata)) {
    if (format_metadata == "csv") {
      path_to_metadata <-
        list.files(path_to_folder,
          pattern = ".csv",
          full.names = T
        )
      metadata <- read.csv(path_to_metadata, dec = dec)
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
    metadata <- read.csv(path_to_metadata, dec = dec)
  }
  metadata <- metadata |>
    rename(File_name = .data[[file_name_column]])

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
    inner_join(distinct(metadata, File_name, .keep_all = TRUE),
      by = c("File_name" = "File_name")) %>%
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

  if(compute_metrics) {
    df_final <- df_final |>
      mutate(x.mm. = x.mm. + center_x, y.mm. = y.mm. + center_y)
      mutate(distance=sqrt(((lag(x.mm.) - x.mm.) ^ 2 + (lag(y.mm.) - y.mm.) ^ 2))) |>
      mutate(speed_mm_s=distance/(X..t.s.-lag(X..t.s.))) |>
      mutate(in_left_half = x.mm. < 0) |>
      mutate(is_immobile = distance == 0) |>  
      mutate(turning_angle = XX)
  }
  res <- list()
  res$resulting_data <- df_final

  res$files_not_in_csv <- files_not_in_csv
  res$files_not_in_metadata <- files_not_in_metadata
  res$duplicate_file_name_in_metadata <- duplicated_file_name

  if (remove_csv_folder) {
    unlink(csv_folder_temp, recursive = TRUE)
  }

  if(return_with_logs){
    return(res)
  } else {
    return(res$resulting_data)
  }
  }


#' Import mitemap from multiple folders
#'
#' @param folders A list of path
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
          path_to_metadata = path_to_metadata[[i]],
          ...
        )
      # if ("MiteMap.y" %in% names(res[[i]]$resulting_data)) {
      #   res[[i]]$resulting_data$MiteMap.y <-
      #     as.character(res[[i]]$resulting_data$MiteMap.y)
      # }
      # if ("MiteMap" %in% names(res[[i]]$resulting_data)) {
      #   res[[i]]$resulting_data$MiteMap <-
      #     as.character(res[[i]]$resulting_data$MiteMap)
      # }
      # if ("Bag" %in% names(res[[i]]$resulting_data)) {
      #   res[[i]]$resulting_data$Bag <-
      #     as.character(res[[i]]$resulting_data$Bag)
      # }
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
