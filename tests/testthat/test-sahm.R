test_that("sahm recession indicator plot works", {
  expect_s3_class(sahm(), "ggplot")
})
