test_that("plots are plots", {

  data <- read_absdata("labour_force")

  expect_s3_class(abs_plot(data = data, indicator = "Employed total", states = "South Australia"), "ggplot")
  expect_s3_class(abs_plot(data = data, indicator = "Employed total", states = "South Australia", markdown = TRUE), "ggplot")

})
