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

