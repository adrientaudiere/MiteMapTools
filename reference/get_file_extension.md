# Get the extension of a file

Internally used in
[`rename_files_with_number()`](https://adrientaudiere.github.io/MiteMapTools/reference/rename_files_with_number.md)
for ex. Warning: don't work when there is '.' in the name of the file
before the extension

## Usage

``` r
get_file_extension(file_path)
```

## Arguments

- file_path:

  (required): path to a file

## Value

The extension of a file.

## Author

Adrien Taudière

## Examples

``` r
get_file_extension("my_file.csv")
#> [1] "csv"
get_file_extension("my.file.csv")
#> ! There is more than one '.' in the file path: my.file.csv
#> [1] "file" "csv" 
get_file_extension("my_file.csv.zip")
#> ! There is more than one '.' in the file path: my_file.csv.zip
#> [1] "csv" "zip"
```
