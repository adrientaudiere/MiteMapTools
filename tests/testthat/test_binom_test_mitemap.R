test_that("binom_test_mitemap works well", {
  expect_equal(dim(suppressWarnings(binom_test_mitemap(MM_data, "Treatment"))), c(3, 8))
})
