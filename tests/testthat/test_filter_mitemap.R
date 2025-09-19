MM_filtered_centered <- filter_mitemap(MM_data, center_x = -20, center_y = -20)


MM_almost_no_filtered <- filter_mitemap(MM_data,
  first_seconds_to_delete = -1,
  max_x_value = 1000,
  max_y_value = 1000,
  bad_range_value_x = 500,
  bad_range_value_y = 500,
  maximum_time = 100000
)

MM_filtered_HH <- filter_mitemap(MM_data,
  HH = MM_ind_data$resulting_data
)


test_that("filter_mitemap works", {
  expect_equal(dim(MM_filtered_centered), c(78785, 12))
  expect_equal(dim(MM_filtered_HH), c(78785, 12))
  expect_equal(dim(MM_almost_no_filtered), c(117582, 12))
})
