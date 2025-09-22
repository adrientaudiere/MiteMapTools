extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  factor = "Treatment", force = TRUE, verbose = FALSE
)

test_that("extract_heatmap works", {
  expect_equal(length(list.files("Heatmap/")), 7)
  expect_equal(length(list.files("Heatmap/", recursive = TRUE)), 54)
})

test_that("extract_heatmap creates expected directory structure", {
  expect_true(dir.exists("Heatmap"))
  
  # Check for subdirectories
  heatmap_dirs <- list.dirs("Heatmap", recursive = FALSE)
  expect_gt(length(heatmap_dirs), 0)
})

test_that("extract_heatmap parameter validation", {
  # Test with different parameters
  expect_no_error({
    extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
      factor = "Treatment", 
      unity = 1,
      wrap = "Treatment",
      force = TRUE, 
      verbose = TRUE
    )
  })
})

test_that("extract_heatmap handles different factor values", {
  # Clean up first
  unlink("Heatmap", recursive = TRUE)
  
  # Test with different wrap parameter if available
  expect_no_error({
    extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
      factor = "Treatment",
      wrap = NULL,
      force = TRUE, 
      verbose = FALSE
    )
  })
  
  expect_true(dir.exists("Heatmap"))
})

unlink("Heatmap", recursive = TRUE)
