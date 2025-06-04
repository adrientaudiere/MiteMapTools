test_that("binom_test_mitemap works well", {
  expect_true(sum(dim(binom_test_mitemap(HH_example$resulting_data)) == c(6, 10)) == 2)
  expect_equal(round(mean(binom_test_mitemap(HH_example$resulting_data, p.adjust_method = "bonferroni")$p.value.adj), 2), 0.34)
  expect_equal(round(mean(binom_test_mitemap(HH_example$resulting_data)$p.value.adj), 2), 0.23)
})
