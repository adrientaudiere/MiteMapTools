test_that("vioplot_mitemap works", {
  expect_s3_class(vioplot_mitemap(MM_data, "Treatment"), "ggplot")
})

test_that("vioplot_mitemap return structure validation", {
  plot_result <- vioplot_mitemap(MM_data, "Treatment")

  expect_s3_class(plot_result, "ggplot")
})

test_that("vioplot_mitemap parameter validation", {
  # Test with different factors if available
  if ("Sex" %in% colnames(MM_data)) {
    expect_s3_class(vioplot_mitemap(MM_data, "Sex"), "ggplot")
  }

  # Test error handling with invalid factor
  expect_error(
    suppressWarnings(vioplot_mitemap(MM_data, "NonExistentFactor"))
  )
})

test_that("vioplot_mitemap handles data structure variations", {
  # Test with direct tibble
  plot1 <- vioplot_mitemap(MM_data, "Treatment")

  # Test with list structure
  mm_list <- list(resulting_data = MM_data)
  plot2 <- vioplot_mitemap(mm_list, "Treatment")

  expect_s3_class(plot1, "ggplot")
  expect_s3_class(plot2, "ggplot")
})
