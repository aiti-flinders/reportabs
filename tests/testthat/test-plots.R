test_that("plots are plots", {
  expect_s3_class(abs_plot(.data = read_absdata("labour_force"), indicator = "Employed total", states = "South Australia"), "ggplot")

})
