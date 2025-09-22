ch <- convex_hull_mitemap(MM_data, plot_show = F)

test_that("convex_hull_mitemap works well", {
  expect_equal(dim(ch), c(219, 7))
})

test_that("convex_hull_mitemap return structure", {
  expect_s3_class(ch, "tbl_df")
  expect_true("File_name" %in% colnames(ch))
  expect_gt(nrow(ch), 0)
})

test_that("convex_hull_mitemap with plot display", {
  # Test with plot_show = TRUE (should not error)
  expect_no_error({
    ch_plot <- convex_hull_mitemap(MM_data, plot_show = TRUE)
  })
})

test_that("convex_hull_mitemap parameter variations", {
  # Test with different parameters
  ch_custom <- convex_hull_mitemap(MM_data, 
                                  plot_show = FALSE,
                                  prop_points = 0.5)
  
  expect_s3_class(ch_custom, "tbl_df")
  expect_gt(nrow(ch_custom), 0)
})

test_that("convex_hull_mitemap handles data structure variations", {
  # Test with direct tibble
  ch1 <- convex_hull_mitemap(MM_data, plot_show = FALSE)
  
  # Test with list structure
  mm_list <- list(resulting_data = MM_data)
  ch2 <- convex_hull_mitemap(mm_list, plot_show = FALSE)
  
  expect_s3_class(ch1, "tbl_df")
  expect_s3_class(ch2, "tbl_df")
  expect_equal(nrow(ch1), nrow(ch2))
})
