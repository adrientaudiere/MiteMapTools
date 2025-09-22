#' Rename files with number inside a folder
#' 
#' @description
#' MiteMap software may export zip files with parenthesis and space in the name
#'  (for example "My file (1).zip", which can be problematic.
#'  This function rename files by adding the number inside the parenthesis 
#'  before the extension (for example "My_file_1.zip"). It also rename the files
#'  inside the zip file to avoid confusion. Note that the name of the csv inside
#'  a zip file with a parenthesis do not have parenthesis into this name. This 
#'  function add the number at the end of the csv file in order to match to
#'  the zip file name.
#'  
#'  If `keep_original` is TRUE, the original zip files with parenthesis are kept.
#'   
#'  Note that this renaming must be also done in the metadata files when using
#'  the [import_mitemap()] function. 
#'
#' @param path_to_folder (required) Path to folder
#' @param keep_original (logical, default FALSE). Do we keep zip_files with
#'  parenthesis in the name?
#'
#' @returns Nothing. Only modify files in a folder.
#' @author Adrien Taudière
#' @export
#'
#' @examples
#' dontrun{
#' rename_files_with_number(system.file("extdata", "mitemap_example", package = "MiteMapTools"))
#' rename_files_with_number(system.file("extdata", "mitemap_example_large", package = "MiteMapTools"),
#' keep_original=TRUE)
#' }
rename_files_with_number <- function(path_to_folder, keep_original = FALSE) {
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
    if (!keep_original) {
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
