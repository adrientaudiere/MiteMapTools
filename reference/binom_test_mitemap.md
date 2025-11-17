# Test binomial on MiteMap for presence in the half part of the odor

Test binomial on MiteMap for presence in the half part of the odor

## Usage

``` r
binom_test_mitemap(
  MiteMap,
  factor = NULL,
  format = "HH",
  verbose = TRUE,
  p.adjust_method = "BH",
  level = "run",
  alternative = "two.sided"
)
```

## Arguments

- MiteMap:

  (required) The result of import_mitemap

- factor:

  (required, default NULL) The column name to separate individuals in
  the MiteMap data frame (e.g., "Treatment").

- format:

  (default "HH") The format of `left` area. "HH" for Half-Half, "CH" for
  Circular-Half.

- verbose:

  (Logical, default = TRUE) If TRUE, the function print additional
  information.

- p.adjust_method:

  (default "BH") method for p-adjustement. See
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

- level:

  (default "run") The level of analysis. "run" to consider each run
  (File_name) as one replicate. "lines" to consider each line (i.e. each
  temporal point) of MiteMap as one replicate. The level run is more
  conservative as it considers that each run is one independent
  replicate. Use level "lines" carefully as it introduce a high
  pseudoreplication risk.

- alternative:

  (default "two.sided") The alternative hypothesis to test. See
  [`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html).

## Value

A tibble of results for binomial test

## Details

The test is run for each factor values with a p-adjustement step. For
each run (filename), the proportion of points in the left half is
calculated. If the proportion is superior to 0.5, the run is considered
as "in left", else "in right". Then a binomial test is run to test if
the proportion of runs "in left" is significantly different from 0.5 for
each levels of the factor. If format is "CH", the same process is done
but for presence in the circular half rather than a half part.

If level is "lines", each line of MiteMap is considered as one replicate
(i.e. the proportion of points in left half is calculated for each line
of MiteMap). This approach is not recommended as it introduce a high
pseudoreplication risk.

## Author

Adrien Taudière

## Examples

``` r

binom_test_mitemap(MM_data, factor = "Treatment")
#> ℹ Running binomial test with format: HH, level: run
#> Warning: There were 18 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `across(...)`.
#> ℹ In group 133: `File_name = "MM012022_05_17_20h27m19s"`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 17 remaining warnings.
#> ✔ Binomial test completed for 3 groups with BH adjustment
#> # A tibble: 3 × 8
#>   Treatment     n   yes    no p.value p.value.adj estimate CI           
#>   <chr>     <int> <int> <int>   <dbl>       <dbl>    <dbl> <chr>        
#> 1 DCM          10     5     5   1           1        0.5   0.187 - 0.813
#> 2 Mix         112    64    48   0.156       0.234    0.571 0.474 - 0.665
#> 3 nothing     129    79    50   0.013       0.039    0.612 0.523 - 0.697
binom_test_mitemap(MM_data, factor = "Treatment", format = "CH")
#> ℹ Running binomial test with format: CH, level: run
#> Warning: There were 18 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `across(...)`.
#> ℹ In group 133: `File_name = "MM012022_05_17_20h27m19s"`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 17 remaining warnings.
#> ✔ Binomial test completed for 3 groups with BH adjustment
#> # A tibble: 3 × 8
#>   Treatment     n   yes    no p.value p.value.adj estimate CI           
#>   <chr>     <int> <int> <int>   <dbl>       <dbl>    <dbl> <chr>        
#> 1 DCM          10     5     5 1          1           0.5   0.187 - 0.813
#> 2 Mix         112    85    27 0.00001    0.000015    0.759 0.669 - 0.835
#> 3 nothing     129    95    34 0.00001    0.000015    0.736 0.652 - 0.81 
binom_test_mitemap(MM_data, factor = "Treatment", level = "lines")
#> ℹ Running binomial test with format: HH, level: lines
#> Warning: There were 18 warnings in `summarise()`.
#> The first warning was:
#> ℹ In argument: `across(...)`.
#> ℹ In group 133: `File_name = "MM012022_05_17_20h27m19s"`.
#> Caused by warning in `min()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 17 remaining warnings.
#> ✔ Binomial test completed for 3 groups with BH adjustment
#> # A tibble: 3 × 8
#>   Treatment     n   yes    no p.value p.value.adj estimate CI           
#>   <chr>     <int> <int> <int>   <dbl>       <dbl>    <dbl> <chr>        
#> 1 DCM          10  6416  7611 0.00001     0.00001    0.457 0.449 - 0.466
#> 2 Mix         112 72443 84648 0.00001     0.00001    0.461 0.459 - 0.464
#> 3 nothing     129 83916 98754 0.00001     0.00001    0.459 0.457 - 0.462

MM_data |>
  filter(Biomol_sp %in% c("DGSS", "DGL1", "D_carpathicus")) |>
  binom_test_mitemap(factor = "Biomol_sp")
#> ℹ Running binomial test with format: HH, level: run
#> ✔ Binomial test completed for 3 groups with BH adjustment
#> # A tibble: 3 × 8
#>   Biomol_sp         n   yes    no p.value p.value.adj estimate CI           
#>   <chr>         <int> <int> <int>   <dbl>       <dbl>    <dbl> <chr>        
#> 1 DGL1             19    14     5   0.064       0.192    0.737 0.488 - 0.909
#> 2 DGSS             67    37    30   0.464       0.607    0.552 0.426 - 0.674
#> 3 D_carpathicus    15     9     6   0.607       0.607    0.6   0.323 - 0.837
```
