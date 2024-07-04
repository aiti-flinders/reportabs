test_that("pdf output works", {
  skip(message = "Skipping render for now")
  expect_type(object = render_monthly_briefing(), type = "logical")
})
