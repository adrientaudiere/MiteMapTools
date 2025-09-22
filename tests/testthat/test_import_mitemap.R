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
