ch <- convex_hull_mitemap(MM_data, plot_show = FALSE, verbose = FALSE)

test_that("convex_hull_mitemap works well", {
  expect_equal(dim(ch), c(219, 7))
})

test_that("convex_hull_mitemap return structure", {
  expect_s3_class(ch, "data.frame")
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
                                  unity=1.2,
                                  tbe=4,
                                  probs_quantile = 0.9)
  
  expect_s3_class(ch_custom, "data.frame")
  expect_gt(nrow(ch_custom), 0)
  
  ch_custom2 <- convex_hull_mitemap(MM_data,
                                    min_nb_spatial_points = 4,
                                    each_point_count_one = TRUE,
                                    plot_center_of_mass = FALSE)
  expect_s3_class(ch_custom2, "data.frame")
  expect_gt(nrow(ch_custom2), 0)
})

test_that("convex_hull_mitemap handles data structure variations", {
  ch1 <- convex_hull_mitemap(MM_data, plot_show = FALSE)
  
  # Test with list structure
  mm_list <- list(resulting_data = MM_data)
  ch2 <- convex_hull_mitemap(mm_list, plot_show = FALSE)
  
  expect_s3_class(ch1, "data.frame")
  expect_s3_class(ch2, "data.frame")
  expect_gt(nrow(ch1), 0)
  expect_gt(nrow(ch2), 0)
})


