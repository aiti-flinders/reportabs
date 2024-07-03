test_that("plots are plots", {
  expect_s3_class(abs_plot(data = read_absdata("labour_force"),
                           v = list(indicator = "Employed total", state = "South Australia")),
                  "ggplot")

})
