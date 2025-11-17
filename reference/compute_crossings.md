# Compute Crossings Between Path Segments

Internally used in function
[`import_mitemap()`](https://adrientaudiere.github.io/MiteMapTools/reference/import_mitemap.md)
to compute the number of crossings in a path. A crossing is defined as
an intersection between two segments of the path. The function can also
compute crossings within a specified time window.

## Usage

``` r
compute_crossings(time_vec, x_vec, y_vec, time_window = NULL)
```

## Arguments

- time_vec:

  (numerical vector) the vector of time

- x_vec:

  (numerical vector) the vector of x coordinates

- y_vec:

  (numerical vector) the vector of y coordinates

- time_window:

  (numerical, default = NULL) If not NULL, only crossings with segments
  that started within the last 'time_window' seconds are counted
  focusing only on recent "interactions" with previous path.

## Value

A list with three elements:

- crossings_at_point: A numerical vector indicating the number of
  crossings at each point in the path.

- crossings_cumsum: A numerical vector representing the cumulative sum
  of crossings up to each point.

- crossings_windowed: A numerical vector indicating the number of
  crossings at each point, considering only segments that started within
  the specified time window.

## Author

Adrien Taudière

## Examples

``` r
time_vec <- c(0, 1, 2, 3, 4, 5)
x_vec <- c(0, 1, 1, 0, -1, -1)
y_vec <- c(0, 0, 1, 1, 0, -1)
result <- compute_crossings(time_vec, x_vec, y_vec, time_window = 3)
result
#> $crossings_at_point
#> [1] 0 0 1 0 0 0
#> 
#> $crossings_cumsum
#> [1] 0 0 1 1 1 1
#> 
#> $crossings_windowed
#> [1] 0 0 1 0 0 0
#> 

MM <- import_mitemap(
  system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  file_name_column = "File (mite ID)", compute_metrics = FALSE
)
#> ℹ Found 54 zip files to process
#> ℹ Reading 54 csv files
#> ! 1390 files described in metadata were not present in the list of csv files
#> ! 1 file present in the list of csv files was not described in metadata
#> ✔ The final number of samples for folder is 76032
#> 
#> ── Data filtering summary ──
#> 
#> ℹ Rows removed (first 2 seconds): 603
#> ℹ Rows removed (bad x range): 2798 (2 runs)
#> ℹ Rows removed (bad x values): 1960
#> ℹ Rows removed (bad y values): 26
#> ✔ Total rows after filtering: 70828 (from 76032)
#> ✔ Total runs after filtering: 50 (from 53)

MM |>
  group_by(File_name) |>
  filter(File_name %in%
    c(
      "MM012022_05_17_10h23m35s",
      "MM012022_05_17_09h23m48s",
      "MM012022_05_17_08h23m53s"
    )) |>
  group_modify(~ {
    result <- compute_crossings(.x$X..t.s., .x$x.mm., .x$y.mm., time_window = 10) #'
    .x %>%
      mutate(
        crossings_at_point = result$crossings_at_point,
        crossings_cumsum = result$crossings_cumsum,
        crossings_windowed = result$crossings_windowed
      )
  }) |>
  group_by(Treatment, File_name) |>
  summarize(n_crossings = max(crossings_cumsum, na.rm = TRUE))
#> `summarise()` has grouped output by 'Treatment'. You can override using the
#> `.groups` argument.
#> # A tibble: 3 × 3
#> # Groups:   Treatment [2]
#>   Treatment File_name                n_crossings
#>   <chr>     <chr>                          <dbl>
#> 1 DCM       MM012022_05_17_10h23m35s         212
#> 2 Mix       MM012022_05_17_08h23m53s         271
#> 3 Mix       MM012022_05_17_09h23m48s         234

if (FALSE) { # \dontrun{
MM |>
  group_by(File_name) |>
  group_modify(~ {
    result <- compute_crossings(.x$X..t.s., .x$x.mm., .x$y.mm., time_window = 10) #'
    .x %>%
      mutate(
        crossings_at_point = result$crossings_at_point,
        crossings_cumsum = result$crossings_cumsum,
        crossings_windowed = result$crossings_windowed
      )
  }) |>
  group_by(Treatment, File_name) |>
  summarize(n_crossings = max(crossings_cumsum, na.rm = TRUE)) |>
  ggstatsplot::ggbetweenstats(Treatment, n_crossings)
} # }
```
