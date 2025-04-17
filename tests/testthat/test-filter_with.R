test_that("filter list defaults", {
  library(reportabs)
  data <- read_labour_force_data(labour_force)
  expect_error(filter_list(data, over = "state"))
  expect_error(filter_list(data, over = list(state = "South Australia")))
})
