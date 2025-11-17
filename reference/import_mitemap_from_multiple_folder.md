# Import mitemap from multiple folders

Import mitemap from multiple folders

## Usage

``` r
import_mitemap_from_multiple_folder(
  folders = NULL,
  path_to_metadata = NULL,
  return_with_logs = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- folders:

  (required) A list of path to folders

- path_to_metadata:

  A list of path to metadata files. Can be left to NULL if metadata
  files are located at the roots of each folders or if no metadata is
  used (`metadata_format`=NULL).

- return_with_logs:

  (Logical, default FALSE). If TRUE, the returning object is a list of 4
  elements containing useful information to explore unmatching name
  between file_names and metadata.

- verbose:

  (logical). If TRUE, print additional information.

- ...:

  Other params for be passed on to
  [`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md)

## Value

A tibble or a list of 4 elements if `return_with_logs` is TRUE. See
?[`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md)

## Author

Adrien Taudière

## Examples

``` r
if (FALSE) { # \dontrun{
mm <- import_mitemap_from_multiple_folder(list(
  system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  system.file("extdata", "mitemap_example_large", package = "MiteMapTools")
))

mm_logs <- import_mitemap_from_multiple_folder(
  c(
    system.file("extdata", "mitemap_example", package = "MiteMapTools"),
    system.file("extdata", "mitemap_example_large", package = "MiteMapTools")
  ),
  return_with_logs = TRUE
)

mm_logs$files_not_in_metadata
} # }
```
