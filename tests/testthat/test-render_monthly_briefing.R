test_that("pdf output works", {
  expect_type(object = render_monthly_briefing(output_format = "html"), type = "NULL")
  file.remove("2025-january-south-australia.html")
})
