MM_filtered_centered <- filter_mitemap(MM_example, center_x = -20, center_y = -20)

test_that("vioplot_mitemap works", {
  expect_s3_class(vioplot_mitemap(MM_filtered_centered), "ggplot")
})
