test_that("Plots can be generated", {
  library(reportabs)
  plot_data <- read_absdata("labour_force_briefing")


  expect_s3_class(
    abs_plot(labour_force_briefing,
             list(indicator = "Employed total",
                  series_type = "Seasonally Adjusted",
                  state = "South Australia")),
    "ggplot")
  expect_s3_class(
    abs_plot(labour_force_briefing,
             list(indicator = "Employed total",
                  series_type = "Seasonally Adjusted",
                  state = "South Australia"),
             compare_aus = FALSE),
    "ggplot")
  expect_s3_class(
    abs_plot(labour_force_briefing,
             list(indicator = c("Employed total", "Employed full-time"),
                  series_type = "Seasonally Adjusted",
                  state = "Australia"),
             compare_aus = FALSE),
    "ggplot")

  # Specifying multiple indicators and multiple other variables shouldn't work unless a facet has been specified
  expect_error(
    abs_plot(labour_force_briefing,
             list(indicator = c("Employed total", "Employed full-time"),
                  series_type = "Seasonally Adjusted",
                  state = "South Australia")
    )
  )

  expect_s3_class(
    abs_plot(labour_force_briefing,
             list(indicator = c("Employed total", "Employed full-time"),
                  series_type = "Seasonally Adjusted",
                  state = "South Australia"),
             facet = "state"
  ),
  "ggplot")

})
