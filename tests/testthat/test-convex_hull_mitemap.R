ch <- convex_hull_mitemap(MM_data, plot_show = F)

test_that("convex_hull_mitemap works well", {
  expect_equal(dim(ch), c(219, 7))
})
