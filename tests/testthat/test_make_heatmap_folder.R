extract_heatmap(system.file("extdata", "mitemap_example", package = "MiteMapTools"),
  factor = "Treatment", force = TRUE, verbose = FALSE
)

test_that("extract_heatmap works", {
  expect_equal(length(list.files("Heatmap/")), 7)
  expect_equal(length(list.files("Heatmap/", recursive = TRUE)), 54)
})
unlink("Heatmap", recursive = TRUE)
