test_that("rename_files_with_number handles files with parentheses", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_rename")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create a mock zip file with parentheses in name
  test_file <- file.path(test_dir, "test_file (1).zip")
  
  # Create a simple text file and zip it
  text_file <- file.path(test_dir, "temp.txt")
  writeLines("test content", text_file)
  
  # Only run test if zip package is available
  skip_if_not_installed("zip")
  skip_if_not_installed("here")
  
  # Create zip file (simplified test - just check that function doesn't error)
  zip::zip(test_file, text_file, mode = "cherry-pick")
  
  # Test that function runs without error
  expect_no_error({
    rename_files_with_number(test_dir, keep_original = TRUE)
  })
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("rename_files_with_number handles directory without parentheses files", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_rename_empty")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create a regular file without parentheses
  regular_file <- file.path(test_dir, "regular_file.txt")
  writeLines("test content", regular_file)
  
  skip_if_not_installed("here")
  
  # Test that function runs without error when no files match pattern
  expect_no_error({
    rename_files_with_number(test_dir, keep_original = FALSE)
  })
  
  # Regular file should still exist
  expect_true(file.exists(regular_file))
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("rename_files_with_number parameter validation", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_param")
  dir.create(test_dir, showWarnings = FALSE)
  
  skip_if_not_installed("here")
  
  # Test with keep_original = FALSE
  expect_no_error({
    rename_files_with_number(test_dir, keep_original = FALSE)
  })
  
  # Test with keep_original = TRUE  
  expect_no_error({
    rename_files_with_number(test_dir, keep_original = TRUE)
  })
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})