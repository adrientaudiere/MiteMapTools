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
