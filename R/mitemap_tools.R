library("tidyverse")
library("readxl")
library("data.table")
library("pracma")
library("plotrix")
library("tibble")

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")

#' import_mitemap
#' @param path_to_folder Path to the folder with zip files containing csv and with one metadata files in the .xlsx format
#' @param path_to_metadata Path to the xlsx file or csv. If there is only one csv file set path_to_metadata="csv" (or xlsx). Need to be set using a complete path to csv file if this file is not in the folder or if their is multiple xlsx/csv files in the folder
#' @param format_metadata Either csv or xlsx
#' @param type_of_files (default : "raw_data") A pattern to select the type of csv (either "raw_data", "CH" or "HH")
#' files using their name
#' @param delete_parenthesis (Logical, default FALSE) Do we delete parenthesis with a number inside in the name of the files. Note that the name of the csv inside a zip file with a parenthesis do not have parenthesis into this name. Thus the default value to TRUE is recommended either in delete_parenthesis or in replace_parenthesis
#' @param replace_parenthesis (Logical, default TRUE) Replace xxx(1) by xxx_1
#' @param delete_space (Logical, default TRUE)
#' @param messages (Logical, default TRUE) Do we print some warnings?
#'
#' @return
#' @export
#'
#' @examples
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
                           dec=","
                          ) {
  if (file.exists("csv_folder")) {
    stop(
      "The folder csv_folder is present in your path workink directory. Please manually delete it."
    )
  }

  zip_files <- list.files(
    path_to_folder,
    pattern = ".zip",
    full.names = T,
    include.dirs = T
  )

  if (length(zip_files) == 0) {
    unlink("csv_folder", recursive = TRUE)

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
      unlink("csv_folder", recursive = TRUE)

      stop(paste(
        "There is multiple csv files for the format.",
        type_of_files,
        sep = ""
      ))
    }
    if (length(file_of_interest > 0)) {
      unzip(zip_files[i], file_of_interest, exdir = "csv_folder")
    }
  }

  file.remove(list.files("csv_folder", pattern = "jpg", full.names = T)) # remove jpg files if present
  file.remove(list.files("csv_folder", pattern = "png", full.names = T)) # remove png files if present
  csv_files <- list.files("csv_folder", full.names = T)


  csv_list <- list()
  for (f in csv_files) {
    csv_list[[f]] <- read.delim(f)
  }
  df <- rbindlist(csv_list, idcol = "Nom_fichier")

  if (type_of_files == "CH") {
    df$Nom_fichier <- gsub(".csv", "",
                           gsub(
                             "_Donnees_traitees_formeC_",
                             "",
                             gsub("csv_folder/", "", df$Nom_fichier)
                           ))
  } else if (type_of_files == "HH") {
    df$Nom_fichier <- gsub(".csv", "",
                           gsub(
                             "_Donnees_traitees_formeD_",
                             "",
                             gsub("csv_folder/", "", df$Nom_fichier)
                           ))
  } else if (type_of_files == "raw_data") {
    df$Nom_fichier <- gsub(".csv", "",
                           gsub(
                             "_donnees_brutes_",
                             "",
                             gsub("csv_folder/", "", df$Nom_fichier)
                           ))
    if (csv_with_correction) {
      df$Nom_fichier <- gsub("_cor", "", df$Nom_fichier)
    }
  } else {
    stop("type_of_files must be one of 'CH', 'HH' or 'raw_data'")
  }


  df <- tibble(df)

  if (is.null(path_to_metadata)) {
    if (format_metadata == "csv") {
      path_to_metadata <-
        list.files(path_to_folder,
                   pattern = ".csv",
                   full.names = T)
      # if(length(path_to_metadata != 1)){
      #   message(path_to_folder)
      #   stop("You must specify a path to metadata because there are multiple csv files in the folder")
      # }
      metadata <- read.csv(path_to_metadata, dec=dec)
    } else if (format_metadata == "xlsx") {
      path_to_metadata <-
        list.files(path_to_folder,
                   pattern = ".xlsx",
                   full.names = T)
        # if(length(path_to_metadata != 1)) {
        #   stop("You must specify a path to metadata because there are multiple xlsx files in the folder")
        # }
      metadata <- read_excel(path_to_metadata)
    }
  } else {
    if (length(path_to_metadata) > 1) {
      stop(
        "There is more than one xlsx/csv file in the folder. Use the path to metadata argument to set the correct path to the metadata file."
      )
    }
    metadata <- read.csv(path_to_metadata, dec=dec)
  }

  if (delete_parenthesis) {
    metadata$nom.du.fichier <-
      gsub("\\([[:xdigit:]]\\)", "", metadata$nom.du.fichier, fixed = F)
  } else if (replace_parenthesis){
    metadata$nom.du.fichier <-
      sub("\\)", "", sub("\\(", "_", metadata$nom.du.fichier))
  }

  if (delete_space) {
    metadata$nom.du.fichier <- gsub(" ", "", metadata$nom.du.fichier)
  }

  duplicated_file_name <-
    metadata$nom.du.fichier [duplicated(metadata$nom.du.fichier )]
  metadata <-
    metadata[!metadata$nom.du.fichier %in% duplicated_file_name, ]
  
  df_final <- df %>%
    inner_join(metadata, by = c("Nom_fichier" = "nom.du.fichier")) %>%
    tibble()

  if(nrow(df_final)==0){
    message("None correspondance between metadata and files names")
    return(NULL)
  }

  files_not_in_metadata <-
    unique(df$Nom_fichier)[!unique(df$Nom_fichier) %in% metadata$nom.du.fichier]
  nb_files_not_in_metadata  <- length(files_not_in_metadata)

  files_not_in_csv <-
    metadata$nom.du.fichier[!metadata$nom.du.fichier %in% df$Nom_fichier]
  nb_files_not_in_csv  <- length(files_not_in_csv)

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

  if (remove_csv_folder){
    unlink("csv_folder", recursive = TRUE)
  }

  return(res)
}


#' make_heatmap_folder
#'
#' @param path_to_folder
#' @param output_path (default =  "Heatmap")
#' @param path_to_metadata
#' @param format_metadata (either csv of xlsx)
#' @param subfolder_by_modality
#' @param subfolder_by_farm
#' @param amount (Logical, default = FALSE) : if TRUE use the column Amount instead of Modality for subfolders
#'
#' @return
#' @export
#'
#' @examples
#'
make_heatmap_folder <- function(path_to_folder,
                                output_path = "Heatmap",
                                path_to_metadata = NULL,
                                format_metadata = "csv",
                                subfolder_by_modality = TRUE,
                                subfolder_by_farm = TRUE,
                                amount = FALSE,
                                dec=",") {
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
                   full.names = T)
      if (length(path_to_metadata) > 1) {
        metadata <- list()
        for (i in 1:length(path_to_metadata)) {
          metadata[[i]] <- read.csv(path_to_metadata[[i]], dec=dec)
        }
        metadata <- metadata |> lapply(function(x) {
          mutate(x, across(everything(), as.character)) })  |>
          bind_rows() |>
          readr::type_convert()

      } else {
        metadata <- read.csv(path_to_metadata, dec=dec)
      }
    } else if (format_metadata == "xlsx") {
      path_to_metadata <-
        list.files(path_to_folder,
                   pattern = ".xlsx",
                   full.names = T)
      if (length(path_to_metadata) > 1) {
        metadata <- list()
        for (i in 1:length(path_to_metadata)) {
          metadata[[i]] <- read_excel(path_to_metadata[[i]])
        }
        metadata <- metadata |> lapply(function(x) {
            mutate(x, across(everything(), as.character)) })  |>
          bind_rows() |>
          readr::type_convert()
      } else {
        metadata <- read_excel(path_to_metadata)
      }
    }

  } else {
    metadata <- read.csv(path_to_metadata, dec=dec)
  }

  dir.create(output_path)

  for (i in 1:length(zip_files)) {
    files_in_zip <- unzip(zip_files[i], list = TRUE)
    heatmap_file <-
      files_in_zip$Name[grepl(".png", files_in_zip$Name)]
    heatmap_name <- gsub("_carte_thermique_", "",
                         gsub(".png", "", heatmap_file))

    if (amount) {
      mod <- na.omit(metadata[match(heatmap_name, metadata$nom.du.fichier ), "Amount"])[1]
    } else {
      mod <- na.omit(metadata[match(heatmap_name, metadata$nom.du.fichier ), "Modalité"])[1]
    }
    farm <- na.omit(metadata[match(heatmap_name, metadata$nom.du.fichier ), "Ferme"])[1]

    if (subfolder_by_modality & !subfolder_by_farm) {
      unzip(zip_files[i],
            heatmap_file,
            exdir = paste(output_path, "/", mod, sep = ""))
    }

    if (!subfolder_by_modality & subfolder_by_farm) {
      unzip(zip_files[i],
            heatmap_file,
            exdir = paste(output_path, "/", farm, sep = ""))
    }

    if (!subfolder_by_modality & !subfolder_by_farm) {
      unzip(zip_files[i],
            heatmap_file,
            exdir = paste(output_path, sep = ""))
    }

    if (subfolder_by_modality & subfolder_by_farm) {
      unzip(zip_files[i],
            heatmap_file,
            exdir = paste(output_path,  "/", farm,  "/", mod, sep = ""))
    }

  }
}

#' make_heatmap_from_multiple_folders
#' @param folders A list of path
make_heatmap_from_multiple_folders <- function(folders, ...) {
  if (length(folders) == 1) {
    make_heatmap_folder(folders, output_path = gsub(".*/|$", "", gsub("/$", "", folders)), ...)
  }
  else {
    for (i in 1:length(folders)) {
      make_heatmap_folder(folders[i], output_path = gsub(".*/|$", "", folders[i]), ...)
    }
  }
}

#' import_mitemap_from_multiple_folder
#'
#' @param folders A list of path
#' @param type_of_files A pattern to select the type of csv (either "raw_data", "CH" or "HH")
#' files using their name (e.g. "CH")
#' @param path_to_metadata A list of path
#' @param verbose #todo
#'
#' @return
#' @export
#'
#' @examples
import_mitemap_from_multiple_folder <-
  function(folders = NULL,
           type_of_files = NULL,
           path_to_metadata = NULL,
           verbose = TRUE,
           ...) {
    res <- list()
    for (i in 1:length(folders)) {
      if(verbose) {
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
      if ("Sachet" %in% names(res[[i]]$resulting_data)) {
        res[[i]]$resulting_data$Sachet <-
          as.character(res[[i]]$resulting_data$Sachet)
      }
    }

    final_res <- list()
    final_res$resulting_data <- res |> lapply(function(x) {
      mutate(x$resulting_data, across(everything(), as.character)) }) |>
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




#
#' Title
#'
#' @param MiteMap (required) The result of import_mitemap ($resulting_data) for raw_data
#' @param HH The result of import_mitemap for raw_data. If HH (and/or CH is not NULL), remove run with (i) time in <0, time out <0 and time_in/(time_in + time_out) > 1
#' @param CH The result of import_mitemap for raw_data. If HH (and/or CH is not NULL), remove run with (i) time in <0, time out <0 and time_in/(time_in + time_out) > 1
#' @param first_seconds_to_delete (default: 2) How many seconds we need to delete?
#' @param bad_range_value_x (default: 42) The range value of x that is superior to what we expect.
#' @param bad_range_value_y (default: 42) The range value of y that is superior to what we expect.
#' @param max_x_value (default: 42) Maximum value for x axis.
#' @param min_x_value (default: 0) Minimum value for x axis.
#' @param max_y_value (default: 42) Maximum value for y axis.
#' @param min_y_value (default: 0) Minimum value for y axis.
#' @param maximum_time (default: 603) Maximum time for one run (in seconds)).
#'
#' @return a filter mitemap of class tibble
#' @export
#'
#' @examples
filter_mitemap <- function(MiteMap,
                           HH = NULL,
                           CH = NULL,
                           first_seconds_to_delete = 2,
                           bad_range_value_x = 42,
                           bad_range_value_y = 42,
                           max_x_value = 42,
                           min_x_value = 0,
                           max_y_value = 42,
                           min_y_value = 0,
                           maximum_time = 603) {
  range_x <- tapply(MiteMap$x.mm.,
                    MiteMap$Nom_fichier,
                    function(xx) {
                      max(xx, na.rm = T) - min(xx, na.rm = T)
                    })
  bad_range_x <- range_x[range_x > bad_range_value_x]

  range_y <- tapply(MiteMap$y.mm.,
                    MiteMap$Nom_fichier,
                    function(xx) {
                      max(xx, na.rm = T) - min(xx, na.rm = T)
                    })
  bad_range_y <- range_y[range_y > bad_range_value_y]

  new_MiteMap1 <- MiteMap %>%
    group_by(Nom_fichier) %>%
    filter(X..t.s. >= first_seconds_to_delete) %>%
    filter(!Nom_fichier %in% names(bad_range_x)) %>%
    filter(!Nom_fichier %in% names(bad_range_y)) %>%
    filter(x.mm. > min_x_value & x.mm. < max_x_value) %>%
    filter(y.mm. > min_y_value & y.mm. < max_y_value) %>%
    mutate(X_tile = round(x.mm., 2)) %>%
    mutate(Y_tile = round(y.mm., 2))

  max_times <-
    tapply(new_MiteMap1$X..t.s., new_MiteMap1$Nom_fichier, max)

  new_MiteMap2 <-
    new_MiteMap1[new_MiteMap1$Nom_fichier %in% names(max_times[max_times < maximum_time]), ]

  if (!is.null(HH)) {
    HH <-
      HH %>%
      mutate(prop_Tin = Tin.s. / (Tout.s. + Tin.s.)) %>%
      filter(prop_Tin < 1) %>%
      filter(Tout.s. > 0) %>%
      filter(Tin.s. > 0)

    new_MiteMap3 <- new_MiteMap2 %>%
      filter(Nom_fichier %in% HH$Nom_fichier)
  }

  if (!is.null(CH)) {
    CH <-
      CH %>%
      mutate(prop_Tin = Tin.s. / (Tout.s. + Tin.s.)) %>%
      filter(prop_Tin < 1) %>%
      filter(Tout.s. > 0) %>%
      filter(Tin.s. > 0)

    if (!is.null(HH)) {
      new_MiteMap3 <- new_MiteMap2 %>%
        filter(Nom_fichier %in% CH$Nom_fichier)
    } else {
      new_MiteMap3 <- new_MiteMap3 %>%
        filter(Nom_fichier %in% CH$Nom_fichier)
    }
  }

  if (!is.null(HH) | !is.null(CH)) {
    message(
      paste(
        "Row removed when clearing the first secondes: ",
        sum(MiteMap$X..t.s. < 2),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x range: ",
        sum(MiteMap$Nom_fichier %in% names(bad_range_x)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y range: ",
        sum(MiteMap$Nom_fichier %in% names(bad_range_y)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x values: ",
        sum(MiteMap$x.mm. < min_x_value |
              MiteMap$x.mm. > max_x_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y values: ",
        sum(MiteMap$y.mm. < min_y_value |
              MiteMap$y.mm. > max_y_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for bad HH and/or CH: ",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap3)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for run with times sup to maximum_time",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap2)),
        "\n",
        sep = ""
      )
    )
    return(new_MiteMap3)
  }
  else{
    message(
      paste(
        "Row removed when clearing the first secondes: ",
        sum(MiteMap$X..t.s. < 2),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x range: ",
        sum(MiteMap$Nom_fichier %in% names(bad_range_x)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y range: ",
        sum(MiteMap$Nom_fichier %in% names(bad_range_y)),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad x values: ",
        sum(MiteMap$x.mm. < min_x_value |
              MiteMap$x.mm. > max_x_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing bad y values: ",
        sum(MiteMap$y.mm. < min_y_value |
              MiteMap$y.mm. > max_y_value),
        "\n",
        sep = ""
      ),
      paste(
        "Row removed when clearing for run with times sup to maximum_time: ",
        sum(nrow(new_MiteMap1) - nrow(new_MiteMap2)),
        "\n",
        sep = ""
      )
    )
    return(new_MiteMap2)
  }

}














# unity (default = 1): taille des carrés de la grille en mm
# tbe = 3 => 25% des données prises en compte?

#' Title
#'
#' @param MiteMap
#' @param unity (default = 1): size of square grid in mm
#' @param tbe (default = 3)
#' @param plot_show (Logical, default = TRUE) : Do we plot all mitemap?
#' @param probs_quantile (default = 0.68)
#' @param min_nb_spatial_points : Minimum number of spatial point to keep the sample
#' @param each_point_count_one (Logical ; default = FALSE)
#' @param hull_col : color of hull area
#' @param plot_center_of_mass (Logical, default = TRUE): Do we plot the center of mass?
#'
#' @return
#' @export
#'
#' @examples
convex_hull_mitemap <- function(MiteMap,
                                unity = 1,
                                tbe = 3,
                                plot_show = TRUE,
                                probs_quantile = 0.68,
                                min_nb_spatial_points = 2,
                                each_point_count_one = FALSE,
                                hull_col = "red",
                                plot_center_of_mass = TRUE) {
  hull_area <- list()
  center_of_mass <- list()
  center_of_area_x <- list()
  center_of_area_y <- list()
  hull_length <- list()

  for (run in unique(MiteMap$Nom_fichier)) {
    x <-
      floor(as.numeric(MiteMap[MiteMap$Nom_fichier == run,]$x.mm.) * unity) /
      unity
    y <-
      floor(as.numeric(MiteMap[MiteMap$Nom_fichier == run,]$y.mm.) * unity) /
      unity
    tbe_table <- table(paste(x, y, sep = ":"))
    if (length(tbe_table) < min_nb_spatial_points) {
      message(
        paste(
          "Pas de convexe Hull trouvé pour l'échantillon:",
          run,
          "(pas assez de points spatiaux différents)"
        )
      )
      next
    }
    message(
      paste(
        "Number of spatial points conserved using tbe = ",
        tbe,
        ": ",
        sum(tbe_table > tbe),
        " (",
        round(sum(tbe_table > tbe) / length(tbe_table) * 100, 2) ,
        " %)",
        sep = ""
      )
    )
    nms <- names(tbe_table)
    coords <-
      matrix(as.numeric(unlist(strsplit(nms, ":"))), ncol = 2, byrow = TRUE)
    if (each_point_count_one) {
      xy <- coords[tbe_table > tbe,]
    } else {
      nms2 <- nms[tbe_table > tbe]
      couple <- paste(x, y, sep = ":")
      couple <- couple[couple %in% nms2]
      xy <-
        matrix(as.numeric(unlist(strsplit(couple, ":"))), ncol = 2, byrow
               = TRUE)
    }

    dst <- rowSums(scale(xy, scale = FALSE) ^ 2)
    idx <- which(dst < quantile(dst, probs = probs_quantile))
    if (length(idx) == 0) {
      message(
        paste(
          "Pas de convexe Hull trouvé pour l'échantillon:",
          run,
          "(pas de valeurs de distance < quantile) "
        )
      )
      next
    }
    idx0 <- chull(xy[idx, 1], xy[idx, 2])
    idx0 <- c(idx0, idx0[1])

    center_of_area_x[[run]] <- mean(xy[, 1])
    center_of_area_y[[run]] <- mean(xy[, 2])


    if (plot_show) {
      plot(
        coords,
        cex = tbe_table / 10,
        col = rgb(0, 0, 0, 0.4),
        xlim = c(-20, 20),
        ylim = c(-20, 20),
        asp = 1
      )
      plotrix::draw.circle(0, 0, 20, col = rgb(0, 0, 0.8, 0.2))
      points(center_of_area_x[[run]],
             center_of_area_y[[run]],
             pch = 19,
             col = hull_col)
      lines(xy[idx[idx0], 1], xy[idx[idx0], 2], lwd = 2, col = hull_col)

      title(run)
    }

    z <- xy[idx[idx0],]
    hull_area[[run]] <- polyarea(z[, 1], z[, 2])
    if (nrow(z) > 2) {
      center_of_mass[[run]] <- pracma::poly_center(z[, 1], z[, 2])
    } else {
      center_of_mass[[run]] <- c(NA, NA)
    }


    hull_length[[run]] <- poly_length(z[, 1], z[, 2])
    if (plot_center_of_mass) {
      points(
        center_of_mass[[run]][1],
        center_of_mass[[run]][2],
        pch = 3,
        col = "black",
        cex = 2
      )
    }

  }

  return(
    data.frame(
      "Nom_fichier" = names(unlist(hull_area)),
      "hull_area" = unlist(hull_area),
      "center_of_area_x" = unlist(center_of_area_x),
      "center_of_area_y" = unlist(center_of_area_y),
      "center_of_mass_x" = unlist(center_of_mass)[seq(1, 2 * length(center_of_mass), by =
                                                        2)],
      "center_of_mass_y" = unlist(center_of_mass)[seq(2, 2 * length(center_of_mass), by =
                                                        2)],
      "hull_length" = unlist(hull_length),
      row.names = names(unlist(hull_area))
    )

  )

}


#' Make violin plot of MiteMap data
#'
#' @param MiteMap
#' @param modality : A name of column present in the MiteMap to separate violin plot.
#' @param wrap : A name of column present in the MiteMap to wrap violin plot.
#' @param ... : additional arguments for function geom_violin()
#'
#' @return
#' @export
#'
#' @examples
vioplot_mitemap <-
  function(MiteMap,
           modality = "Modality",
           wrap = "Farm",
           ...) {
    modality_interm <-
      eval(parse(text = paste("MiteMap$", modality, sep = "")))
    MiteMap$Modality <- modality_interm

    wrap_interm <-
      eval(parse(text = paste("MiteMap$", wrap, sep = "")))
    MiteMap$Wrap <- wrap_interm

    ggplot(MiteMap,
           aes(
             x = x.mm.,
             y = factor(Modality),
             color = factor(Modality)
           )) +
      geom_violin(...) +
      ggtitle(paste("Position on x axis in function of",  modality)) +
      facet_wrap(~ Wrap)
  }







#' Test binomial on MiteMap
#'
#' @param MiteMap
#'
#' @return
#' @export
#'
#' @examples

binom_test_mitemap <- function(MiteMap, factor, method="BH") {
  MiteMap_bin = MiteMap %>%
    dplyr::group_by(Farm, .data[[factor]]) %>%
    dplyr::summarise(
      n = n(),
      HH = mean(Binary_choice),
      yes = sum(Binary_choice),
      no = n - yes
    )

  CI <- apply(MiteMap_bin, 1, function(xx) {
    paste(round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$conf.int[1],
      3
    ),
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$conf.int[2],
      3
    ),
    sep = " - ")
  })

  estimate <- apply(MiteMap_bin, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$estimate,
      3
    )
  })

  p.value <- apply(MiteMap_bin, 1, function(xx) {
    round(
      binom.test(
        as.integer(xx["yes"]),
        as.integer(xx["no"]) + as.integer(xx["yes"]),
        alternative = "two.sided"
      )$p.value,
      3
    )
  })

  p.value[p.value == 0] <- 0.001
  p.value.adj <- p.adjust(p.value, method=method)
  
  NewMiteMap_bin <-
    MiteMap_bin %>% add_column(p.value = p.value,
                               p.value.adj = p.value.adj,
                               estimate = estimate,
                               CI = CI)
  return(NewMiteMap_bin)
}


# From the rcompanion package (Salvatore S. Mangiafico ; http://rcompanion.org/handbook/)
fullPTable <-  function(PT)
  {
    PTa <- rep(c(NA_real_),length(PT[,1]))         # Add a row
    PT1 <- rbind(PTa, PT)
    rownames(PT1)[1] <- colnames(PT1)[1]
    PTb <- rep(c(NA_real_),length(PT1[,1]))        # Add a column
    PT1 <- cbind(PT1, PTb)
    n <- length(PT1[,1])
    colnames(PT1)[n] <- rownames(PT1)[n]
    PT2 = t(PT1)                                   # Create new transposed
    PT2[lower.tri(PT2)] = PT1[lower.tri(PT1)]
    diag(PT2) = signif(1.00, digits = 4)           # Set the diagonal
    PT2
  }



rename_files_with_number <- function(path_to_folder, remove_zip_files_with_parenthesis = TRUE) {
  old_wd <- getwd()
  folders_to_renamed <- list.files(here::here(path_to_folder),
     pattern =  "\\([[:xdigit:]]\\)", 
     include.dirs = TRUE,
     full.names = TRUE)
  
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
    if(remove_zip_files_with_parenthesis) {
      lapply(folders_to_renamed, unlink)
    }
  }
  setwd(old_wd)
  unlink("unzip_temp", recursive = TRUE, force = TRUE)
}


get_file_extension <- function(file_path) {
  file_ext <- strsplit(basename(file_path), ".", fixed = TRUE)[[1]][-1]
  return(file_ext)
}


