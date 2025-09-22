MM_data_messy <- import_mitemap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
                                clean=FALSE, verbose = FALSE)

MM_filtered <- filter_mitemap(MM_data_messy, verbose = FALSE)

MM_almost_no_filtered <- filter_mitemap(MM_data_messy,
  first_seconds_to_delete = -1,
  max_x_value = 1000,
  max_y_value = 1000,
  bad_range_value_x = 500,
  bad_range_value_y = 500,
  maximum_time = 1000, verbose = FALSE
)

test_that("filter_mitemap works", {
  expect_equal(dim(MM_filtered), c(70828, 35))
  expect_equal(dim(MM_data_messy), c(76032, 35))
  expect_equal(dim(MM_almost_no_filtered), c(74545, 35))
})

test_that("filter_mitemap parameter validation", {
  # Test with different parameter combinations
  MM_custom1 <- filter_mitemap(MM_data_messy,
    first_seconds_to_delete = 1,
    bad_range_value_x = 50,
    bad_range_value_y = 50,
    verbose = FALSE
  )
  
  expect_s3_class(MM_custom1, "tbl_df")
  expect_lte(nrow(MM_custom1), nrow(MM_data_messy))
})

test_that("filter_mitemap handles extreme filtering", {
  # Test with very restrictive parameters
  MM_restrictive <- filter_mitemap(MM_data_messy,
    first_seconds_to_delete = 10,
    max_x_value = 5,
    max_y_value = 5,
    min_x_value = -5,
    min_y_value = -5,
    verbose = FALSE
  )
  
  expect_s3_class(MM_restrictive, "tbl_df")
  expect_lte(nrow(MM_restrictive), nrow(MM_filtered))
})

test_that("filter_mitemap handles center parameters", {
  # Test with centering
  MM_centered <- filter_mitemap(MM_data_messy,
    center_x = 10,
    center_y = 5,
    verbose = FALSE
  )
  
  expect_s3_class(MM_centered, "tbl_df")
  expect_true("x.mm." %in% colnames(MM_centered))
  expect_true("y.mm." %in% colnames(MM_centered))
})

test_that("filter_mitemap return structure", {
  expect_s3_class(MM_filtered, "tbl_df")
  expect_true(all(c("x.mm.", "y.mm.", "X..t.s.", "File_name") %in% colnames(MM_filtered)))
  
  # Check that filtering actually removes data points
  expect_lt(nrow(MM_filtered), nrow(MM_data_messy))
})
