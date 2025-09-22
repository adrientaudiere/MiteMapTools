mm_csv <- import_mitemap(
  system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  file_name_column = "File (mite ID)", verbose = FALSE
)

mm_csv_log <- import_mitemap(
  system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  file_name_column = "File (mite ID)", verbose = FALSE, return_with_logs = TRUE
)

test_that("import_mitemap is running well", {
  expect_equal(dim(mm_csv), c(70828, 35))
  expect_equal(length(mm_csv_log), 4)
})

test_that("import_mitemap handles different file_name_column", {
  # Test with default file name column
  mm_default <- import_mitemap(
    system.file("extdata", "mitemap_example", package = "MiteMapTools"),
    verbose = FALSE
  )
  
  expect_s3_class(mm_default, "tbl_df")
  expect_gt(nrow(mm_default), 0)
  expect_true("File_name" %in% colnames(mm_default))
})

test_that("import_mitemap return structure validation", {
  # Test return structure with logs
  expect_type(mm_csv_log, "list")
  expect_true("resulting_data" %in% names(mm_csv_log))
  expect_s3_class(mm_csv_log$resulting_data, "tbl_df")
  
  # Test direct return structure
  expect_s3_class(mm_csv, "tbl_df")
  expect_true(all(c("x.mm.", "y.mm.", "X..t.s.", "File_name") %in% colnames(mm_csv)))
})

test_that("import_mitemap handles clean parameter", {
  # Test with clean = FALSE
  mm_messy <- import_mitemap(
    system.file("extdata", "mitemap_example", package = "MiteMapTools"),
    clean = FALSE, verbose = FALSE
  )
  
  expect_s3_class(mm_messy, "tbl_df")
  expect_gt(nrow(mm_messy), nrow(mm_csv)) # Should have more rows when not cleaned
})

test_that("import_mitemap parameter validation", {
  # Test with different parameter combinations
  expect_no_error({
    import_mitemap(
      system.file("extdata", "mitemap_example", package = "MiteMapTools"),
      verbose = TRUE, 
      clean = TRUE,
      compute_metrics = FALSE
    )
  })
})
