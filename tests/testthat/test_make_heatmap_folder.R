make_heatmap(system.file("extdata", "POUL6", package = "MiteMapTools"))

test_that("make_heatmap works", {
  expect_equal(length(list.files("Heatmap/")), 2)
  expect_equal(length(list.files("Heatmap/", recursive = TRUE)), 43)
})
unlink("Heatmap", recursive = TRUE)
