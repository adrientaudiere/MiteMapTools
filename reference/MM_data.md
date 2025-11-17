# example dataset from MiteMapTools

Obtained using code : MM_data \<- import_mitemap( system.file("extdata",
"mitemap_example_large", package = "MiteMapTools"), file_name_column =
"File (mite ID)", verbose = FALSE ) save(MM_data,
file="data/MM_data.rda", compress = "bzip2")

## Usage

``` r
data(MM_data)
```

## Format

A tibble.

## Source

<https://onlinelibrary.wiley.com/doi/10.1002/jez.2651>
