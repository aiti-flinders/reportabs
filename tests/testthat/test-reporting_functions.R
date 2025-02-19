test_that("reporting functions return the correct types", {
  lf <- read_absdata("labour_force")
  filter_list <- list(indicator = "Employed total", sex = "Persons", state = "Australia", series_type = "Trend")
  expect_type(value_at(lf,  filter_list), "double")
  expect_type(last_value(lf, filter_list), "character")
  expect_type(change(lf, filter_list), "character")
  expect_match(change(lf, filter_list), "increased|decreased|to")
  expect_type(average_over(lf, filter_list, as.Date(c("2019-01-01","2020-01-01"))), "double")
})
