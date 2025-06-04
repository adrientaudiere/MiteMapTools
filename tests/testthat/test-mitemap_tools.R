test_that("get_file_extension works", {
  expect_equal(get_file_extension("my_file.csv"), "csv")
})