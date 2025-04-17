
test_that("string formats print the right symbols", {
  expect_match(as_percent(5), "%")
  expect_match(as_percentage_point(5), " percentage points")
  expect_match(as_comma(5), "")
  expect_match(as_comma(5000), ",")
  expect_match(as_comma(1e6), " million")
  expect_match(as_comma(1e9), ",| million")
})

test_that("as_comma_group returns a correctly formatted vector", {
  acg_test <- data.frame(group = c("1", "2", "3", "4"), value = c(1, 100, 1000, 1e6))
  expect_equal(length(as_comma_group(acg_test, group = "group", value = "value")), 4)
  expect_equal(ignore_attr = TRUE, as_comma_group(acg_test, group = "group", value = "value"), c("1", "100", "1,000", "1.0 million"))
  expect_equal(ignore_attr = TRUE, as_comma_group(acg_test, group = "group", value = "value"), as_comma(acg_test, group = "group", value = "value"))
})

test_that("guard rails prevent bad arguments", {
  expect_error(as_comma(5, value = "value"))

})

