mm_csv <- suppressWarnings(import_mitemap(
  system.file("extdata", "POUL6", package = "MiteMapTools")
))
dim(mm_csv$resulting_data)

test_that("import_mitemap is running well", {
  expect_equal(length(mm_csv), 4)
  expect_equal(dim(mm_csv$resulting_data), c(117721, 12))
})
