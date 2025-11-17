# Create folder(s) with heatmaps extracted from zip files optionally organized by a factor

Create folder(s) with heatmaps extracted from zip files optionally
organized by a factor

## Usage

``` r
extract_heatmap(
  path_to_folder,
  factor = NULL,
  output_path = "Heatmap",
  verbose = TRUE,
  force = FALSE,
  heatmap_file_name = "_carte_thermique_",
  ...
)
```

## Arguments

- path_to_folder:

  Path to folder

- factor:

  (character, default NULL) Set to a column name to create subfolder for
  each levels of the modality. If left to NULL, all heatmaps are
  extracted in the same folder.

- output_path:

  (default = "Heatmap")

- verbose:

  (Logical, default = TRUE) If TRUE, the function print additional
  information.

- force:

  Force overwriting the path to output_path

- heatmap_file_name:

  Name of the heatmap file in the zip file. Default is
  "*carte_thermique*" (sorry for french).

- ...:

  Other params for be passed on to
  [`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md)

## Value

Nothing. Create files in folders.

## Author

Adrien Taudière

## Examples

``` r
if (FALSE) { # \dontrun{
extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  factor = "Treatment"
)
extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  factor = "Biomol_sp", force = TRUE
)
extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  clean = FALSE, force = TRUE
)
unlink("Heatmap", recursive = TRUE)
} # }
```
