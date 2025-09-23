test_that("binom_test_mitemap works well", {
  expect_equal(dim(suppressWarnings(binom_test_mitemap(MM_data, "Treatment"))), c(3, 8))
})

test_that("binom_test_mitemap return structure", {
  result <- suppressWarnings(binom_test_mitemap(MM_data, "Treatment"))

  expect_s3_class(result, "tbl_df")
  expect_equal(colnames(result), c(
    "Treatment", "n", "yes", "no", "p.value", "p.value.adj", "estimate",
    "CI"
  ))
  expect_gt(nrow(result), 0)
})

test_that("binom_test_mitemap parameter validation", {
  # Test with different parameters
  expect_no_error({
    suppressWarnings(binom_test_mitemap(MM_data, "Treatment", format = "HH"))
  })

  expect_no_error({
    suppressWarnings(binom_test_mitemap(MM_data, "Treatment", format = "CH"))
  })
})

test_that("binom_test_mitemap error handling", {
  # Test with invalid factor
  expect_error({
    suppressWarnings(binom_test_mitemap(MM_data, "NonExistentColumn"))
  })

  # Test with invalid format
  expect_error({
    suppressWarnings(binom_test_mitemap(MM_data, "Treatment", format = "invalid"))
  })
})
