test_that("reporting functions return the correct types", {
  lf <- read_absdata("labour_force")
  filter_list <- list(indicator = "Employed total")
  expect_type(value_at(lf,  filter_list), "double")
  expect_type(last_value(lf, filter_list), "character")
  expect_type(change(lf, filter_list), "character")
  expect_match(change(lf, filter_list), "increased|decreased|to")
  expect_type(average_over(lf, filter_list, c(2019,2020)), "double")
})
