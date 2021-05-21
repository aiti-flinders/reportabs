test_that("pdf output works", {
  expect_type(object = render_monthly_briefing(), type = "logical")
})
