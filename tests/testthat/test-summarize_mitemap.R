test_that("summarize_mitemap basic functionality works", {
  summary_result <- suppressWarnings(summarize_mitemap(MM_data))
  
  expect_s3_class(summary_result, "tbl_df")
  expect_gt(nrow(summary_result), 0)
  expect_true("File_name" %in% colnames(summary_result))
  expect_true("total_points" %in% colnames(summary_result))
})

test_that("summarize_mitemap creates expected columns", {
  summary_result <- suppressWarnings(summarize_mitemap(MM_data))
  
  # Check for expected summary columns
  expected_cols <- c("File_name", "total_points")
  expect_true(all(expected_cols %in% colnames(summary_result)))
  
  # Check that numeric summary columns are created
  # Should have columns with suffixes: _mean, _sd, _min, _max
  summary_cols <- colnames(summary_result)
  expect_true(any(grepl("_mean$", summary_cols)))
  expect_true(any(grepl("_sd$", summary_cols)))
  expect_true(any(grepl("_min$", summary_cols)))
  expect_true(any(grepl("_max$", summary_cols)))
})

test_that("summarize_mitemap works with custom num_cols", {
  custom_cols <- c("distance_from_previous", "speed_mm_s")
  summary_result <- suppressWarnings(summarize_mitemap(MM_data, num_cols = custom_cols))
  
  expect_s3_class(summary_result, "tbl_df")
  expect_gt(nrow(summary_result), 0)
  
  # Check that summary columns for custom columns are created
  summary_cols <- colnames(summary_result)
  expect_true(any(grepl("distance_from_previous_mean", summary_cols)))
  expect_true(any(grepl("speed_mm_s_mean", summary_cols)))
})

test_that("summarize_mitemap handles empty num_cols", {
  summary_result <- summarize_mitemap(MM_data, num_cols = character(0))
  
  expect_s3_class(summary_result, "tbl_df")
  expect_gt(nrow(summary_result), 0)
  expect_true("File_name" %in% colnames(summary_result))
  expect_true("total_points" %in% colnames(summary_result))
})

test_that("summarize_mitemap one row per file", {
  summary_result <- suppressWarnings(summarize_mitemap(MM_data))
  
  # Number of rows should equal number of unique files
  n_files <- length(unique(MM_data$File_name))
  expect_equal(nrow(summary_result), n_files)
  
  # File names should be unique
  expect_equal(length(unique(summary_result$File_name)), nrow(summary_result))
})

test_that("summarize_mitemap handles data structure variations", {
  # Test with direct tibble input 
  summary_result1 <- suppressWarnings(summarize_mitemap(MM_data))
  
  # Test with list structure (should extract resulting_data)
  mm_list <- list(resulting_data = MM_data)
  summary_result2 <- suppressWarnings(summarize_mitemap(mm_list))
  
  expect_s3_class(summary_result1, "tbl_df")
  expect_s3_class(summary_result2, "tbl_df")
  expect_equal(nrow(summary_result1), nrow(summary_result2))
})