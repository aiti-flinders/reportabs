test_that("filter list defaults", {
  library(reportabs)
  data <- read_absdata("labour_force")
  expect_error(filter_list(data, over = "state"))
  expect_error(filter_list(data, over = list(state = "South Australia")))
})
