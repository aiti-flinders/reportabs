test_that("plots are plots", {
  expect_s3_class(abs_plot(indicator = "Employed total", states = "South Australia"), "ggplot")
})
