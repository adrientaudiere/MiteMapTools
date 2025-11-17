# Rename files with number inside a folder

MiteMap software may export zip files with parenthesis and space in the
name (for example "My file (1).zip", which can be problematic. This
function rename files by adding the number inside the parenthesis before
the extension (for example "My_file_1.zip"). It also rename the files
inside the zip file to avoid confusion. Note that the name of the csv
inside a zip file with a parenthesis do not have parenthesis into this
name. This function add the number at the end of the csv file in order
to match to the zip file name.

If `keep_original` is TRUE, the original zip files with parenthesis are
kept.

Note that this renaming must be also done in the metadata files when
using the
[`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md)
function.

## Usage

``` r
rename_files_with_number(path_to_folder, keep_original = FALSE)
```

## Arguments

- path_to_folder:

  (required) Path to folder

- keep_original:

  (logical, default FALSE). Do we keep zip_files with parenthesis in the
  name?

## Value

Nothing. Only modify files in a folder.

## Author

Adrien Taudière

## Examples

``` r
if (FALSE) { # \dontrun{
rename_files_with_number(system.file("extdata", "mitemap_example", package = "MiteMapTools"))
rename_files_with_number(system.file("extdata", "mitemap_example_large", package = "MiteMapTools"),
  keep_original = TRUE
)
} # }
```
