# Make violin plot of MiteMap data

Make violin plot of MiteMap data

## Usage

``` r
vioplot_mitemap(MiteMap, factor = NULL, wrap = NULL, prop_points = NULL)
```

## Arguments

- MiteMap:

  (required) The result of import_mitemap

- factor:

  : A name of column present in the MiteMap to separate violin plot.

- wrap:

  : A name of column present in the MiteMap to wrap violin plot.

- prop_points:

  (numeric between 0 and 1, default=NULL) If not NULL, a proportion of
  points is randomly sampled and added to the violin plot. This can help
  to visualize the data distribution, especially with large datasets.

## Value

A ggplot object

## Author

Adrien Taudière

## Examples

``` r
vioplot_mitemap(MM_data, "Treatment")

vioplot_mitemap(MM_data, factor = "Treatment", wrap = "Biomol_sp") +
  geom_boxplot(col = "gray60", width = 0.1)


vioplot_mitemap(MM_data, "Treatment", prop_points = 0.01)
```
