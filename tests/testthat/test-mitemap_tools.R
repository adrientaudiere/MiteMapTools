test_that("get_file_extension works", {
  expect_equal(get_file_extension("my_file.csv"), "csv")
})

test_that("get_file_extension handles different extensions", {
  expect_equal(get_file_extension("test.txt"), "txt")
  expect_equal(get_file_extension("data.xlsx"), "xlsx")
  expect_equal(get_file_extension("archive.zip"), "zip")
  expect_equal(get_file_extension("image.png"), "png")
})

test_that("get_file_extension handles files with paths", {
  expect_equal(get_file_extension("/path/to/file.csv"), "csv")
  expect_equal(get_file_extension("folder/subfolder/data.txt"), "txt")
})

test_that("get_file_extension warns about multiple dots", {
  expect_warning(get_file_extension("my.file.csv"), "more than one")
  # But should still return the extension
  expect_equal(suppressWarnings(get_file_extension("my.file.csv")), "csv")
})

test_that("get_file_extension errors when no extension", {
  expect_error(get_file_extension("no_extension"), "no '\\.' inside")
  expect_error(get_file_extension("folder/file_without_ext"), "no '\\.' inside")
})

test_that("get_file_extension handles edge cases", {
  # File with just extension
  expect_equal(get_file_extension(".hidden"), "hidden")
  
  # File with uppercase extension
  expect_equal(get_file_extension("FILE.CSV"), "CSV")
  
  # File with numbers in extension
  expect_equal(get_file_extension("backup.tar.gz"), "gz")
})