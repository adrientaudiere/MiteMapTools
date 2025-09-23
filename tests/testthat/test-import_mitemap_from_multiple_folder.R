test_that("import_mitemap_from_multiple_folder basic functionality", {
  # Test with single folder
  folders <- list(system.file("extdata", "mitemap_example", package = "MiteMapTools"))
  
  result <- import_mitemap_from_multiple_folder(folders, verbose = FALSE)
  
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true("File_name" %in% colnames(result))
})

test_that("import_mitemap_from_multiple_folder with return_with_logs", {
  # Test with single folder and logs
  folders <- list(system.file("extdata", "mitemap_example", package = "MiteMapTools"))
  
  result <- import_mitemap_from_multiple_folder(folders, 
                                               return_with_logs = TRUE, 
                                               verbose = FALSE)
  
  expect_type(result, "list")
  expect_true("resulting_data" %in% names(result))
  expect_true("files_not_in_csv" %in% names(result))
  expect_true("files_not_in_metadata" %in% names(result))
  expect_true("duplicate_file_name_in_metadata" %in% names(result))
  
  expect_s3_class(result$resulting_data, "tbl_df")
  expect_gt(nrow(result$resulting_data), 0)
})

test_that("import_mitemap_from_multiple_folder handles empty folders list", {
  # Test with empty folders
  expect_error({
    import_mitemap_from_multiple_folder(list(), verbose = FALSE)
  })
})

test_that("import_mitemap_from_multiple_folder parameter validation", {
  folders <- list(system.file("extdata", "mitemap_example", package = "MiteMapTools"))
  
  # Test with different verbose settings
  expect_no_error({
    suppressWarnings(import_mitemap_from_multiple_folder(folders, verbose = TRUE, force = TRUE))
  })
  
  expect_no_error({
    import_mitemap_from_multiple_folder(folders, verbose = FALSE, force = TRUE)
  })
})