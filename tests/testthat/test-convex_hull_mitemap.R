MM <- filter_mitemap(MM_example,
  center_x = -20, center_y = -20,
  bad_range_value_x = 50, bad_range_value_y = 50
)
ch <- convex_hull_mitemap(MM, plot_show = F)

test_that("convex_hull_mitemap works well", {
  expect_equal(nrow(ch), 35)
  expect_equal(ncol(ch), 7)
})
