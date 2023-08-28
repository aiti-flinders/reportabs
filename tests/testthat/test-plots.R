test_that("plots are plots", {

  data <- read_absdata("labour_force")

  expect_s3_class(abs_plot_line(data = data, indicator = "Employed total", state = "South Australia"), "ggplot")
  expect_s3_class(abs_plot_line(data = data, indicator = "Employed total", state = "South Australia", markdown = TRUE), "ggplot")

})
