library(reportabs)
library(tibble)

test_that("string formats print the right symbols", {
  expect_match(as_percent(5), "%")
  expect_match(as_percentage_point(5), " percentage points")
  expect_match(as_comma(5), "")
  expect_match(as_comma(5000), ",")
  expect_match(as_comma(1e6), " million")
  expect_match(as_comma(1e9), ",| million")
})

test_that("as_comma_group returns a vector the same length as the input", {
  acg_test <- tribble(~group, ~value, "1", 1, "2", 100, "3", 1000, "4", 1e6)
  expect_equal(length(as_comma_group(acg_test, group = "group", value = "value")), 4)
})

