test_that("vioplot_mitemap works", {
  expect_s3_class(vioplot_mitemap(MM_data, "Treatment"), "ggplot")
})
