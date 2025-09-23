test_that("plot_ind_mitemap basic functionality works", {
  # Test basic plot generation
  plots <- plot_ind_mitemap(MM_data, ind_index = 1)

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_ind_mitemap works with file_names parameter", {
  # Get first file name from MM_data
  first_file <- unique(MM_data$File_name)[1]
  plots <- plot_ind_mitemap(MM_data, file_names = first_file)

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_ind_mitemap works with multiple indices", {
  plots <- plot_ind_mitemap(MM_data, ind_index = c(1, 2))

  expect_type(plots, "list")
  expect_length(plots, 2)
  expect_s3_class(plots[[1]], "ggplot")
  expect_s3_class(plots[[2]], "ggplot")
})

test_that("plot_ind_mitemap works with base circle option", {
  plots <- plot_ind_mitemap(MM_data, ind_index = 1, add_base_circle = TRUE)

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_ind_mitemap works with odor source label", {
  plots <- plot_ind_mitemap(MM_data,
    ind_index = 1,
    add_odor_source = TRUE,
    label_odor_source = "Test Odor"
  )

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_ind_mitemap works without odor source", {
  plots <- plot_ind_mitemap(MM_data, ind_index = 1, add_odor_source = FALSE)

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_ind_mitemap handles custom parameters", {
  plots <- plot_ind_mitemap(MM_data,
    ind_index = 1,
    linewidth = 2.0,
    diameter_base_circle = 50,
    npoints_base_circle = 50
  )

  expect_type(plots, "list")
  expect_length(plots, 1)
  expect_s3_class(plots[[1]], "ggplot")
})
