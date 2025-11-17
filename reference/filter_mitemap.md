# Filter (clean) MiteMap result

This function filter MiteMap data according to several parameters. It
removes: the first seconds of each run, runs with aberrant range of x or
y values, points with aberrant x or y values, runs with a total time
superior to maximum_time. It can also center the x and y values by
additioning a constant to all x and y values. By default the center of
the arena is located at (0,0) with a diameter of 40mm.

## Usage

``` r
filter_mitemap(
  MiteMap,
  first_seconds_to_delete = 2,
  bad_range_value_x = 42,
  bad_range_value_y = 42,
  max_x_value = 21,
  min_x_value = -21,
  max_y_value = 21,
  min_y_value = -21,
  maximum_time = 302,
  center_x = 0,
  center_y = 0,
  verbose = TRUE
)
```

## Arguments

- MiteMap:

  (required) The result of import_mitemap (\$resulting_data) for
  raw_data

- first_seconds_to_delete:

  (default: 2) How many seconds to delete?

- bad_range_value_x:

  (default: 45) The range value of x that is superior to what we expect.
  This param filter values on the File_name basis.

- bad_range_value_y:

  (default: 45) The range value of y that is superior to what we expect.
  This param filter values on the File_name basis.

- max_x_value:

  (default: 21) Maximum value for x axis. This param filter values on
  the time-point (one line) basis.

- min_x_value:

  (default: -21) Minimum value for x axis. This param filter values on
  the time-point (one line) basis.

- max_y_value:

  (default: 21) Maximum value for y axis. This param filter values on
  the time-point (one line) basis.

- min_y_value:

  (default: -21) Minimum value for y axis. This param filter values on
  the time-point (one line) basis.

- maximum_time:

  (default: 603) The final value of time (in seconds)). This param
  filter values on the File_name basis.

- center_x:

  (int, default 0) Center the value of x by additioning center_x mm to
  x.mm.

- center_y:

  (int, default 0) Center the value of y by additioning center_y mm to
  y.mm.

- verbose:

  (Logical, default = TRUE) If TRUE, the function print additional
  information.

## Value

a filtered tibble

## Details

The order of filtering is:

1.  Remove runs (filename) with a total time superior to 'maximum_time'.

2.  Remove the first 'first_seconds_to_delete' seconds of each run.

3.  Remove runs (filename) with aberrant range of x or y values (i.e.
    superior to 'bad_range_value_x' or 'bad_range_value_y').

4.  Remove points with aberrant x or y values (i.e. x.mm. \<
    'min_x_value' or x.mm. \> 'max_x_value', y.mm. \< 'min_y value' or
    y.mm. \> 'max_y_value').

5.  Center the x and y values by additioning 'center_x' and 'center_y'
    to all x and y values.

## Author

Adrien Taudière

## Examples

``` r
mm_csv <- import_mitemap(
  system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  file_name_column = "File (mite ID)", verbose = FALSE, clean = FALSE,
  compute_metrics = FALSE
)
dim(mm_csv)
#> [1] 76032    24

MM_filtered_1 <- filter_mitemap(mm_csv)
#> 
#> ── Data filtering summary ──
#> 
#> ℹ Rows removed (first 2 seconds): 603
#> ℹ Rows removed (bad x range): 2798 (2 runs)
#> ℹ Rows removed (bad x values): 1960
#> ℹ Rows removed (bad y values): 26
#> ✔ Total rows after filtering: 70828 (from 76032)
#> ✔ Total runs after filtering: 50 (from 53)
dim(MM_filtered_1)
#> [1] 70828    24

MM_filtered_2 <- filter_mitemap(mm_csv,
  bad_range_value_x = 41,
  bad_range_value_y = 44,
  first_seconds_to_delete = 1,
  maximum_time = 301.1
)
#> 
#> ── Data filtering summary ──
#> 
#> ℹ Rows removed (run with times > maximum_time): 12941 (9 runs)
#> ℹ Rows removed (first 1 second): 359
#> ℹ Rows removed (bad x range): 2798 (2 runs)
#> ℹ Rows removed (bad x values): 1960
#> ℹ Rows removed (bad y values): 26
#> ✔ Total rows after filtering: 58177 (from 76032)
#> ✔ Total runs after filtering: 41 (from 53)
dim(MM_filtered_2)
#> [1] 58177    24
```
