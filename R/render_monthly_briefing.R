#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param out_dir Directory to save the rendered documents
#' @param state String. Name of state (in full). Defaults to South Australia.T
#' @param years Number. Included graphs are drawn from January of the year specified. Minimum 1978, and defaults to 2017.
#' @param series_type String. Seasonally Adjusted
#' @param input Path to the RMarkdown file which generates the .pdf.
#' Defaults to the monthly_briefing file included in the reportabs package.

#' @return One .pdf document
#' @export render_monthly_briefing
#'
#'
#'
render_monthly_briefing <- function(out_dir = "out",
                                    input = system.file("markdown", "monthly_briefing.Rmd", package = 'reportabs'),
                                    state = "South Australia",
                                    years = 2017,
                                    series_type = "Trend") {

  if (!rlang::is_installed("strayr") || !rlang::is_installed("ragg")) {
    stop("In order to render the monthly briefing document, the following packages must be installed:")
  }

  data <- list("labour_force" = read_absdata("labour_force") |>
                 tidyr::pivot_wider(id_cols = c(date, sex, state, series_type, unit, age),
                                    names_from = indicator,
                                    values_from = value) |>
                 dplyr::mutate(`Underutilised total` = `Underemployed total` + `Unemployed total`) |>
                 tidyr::pivot_longer(cols = "Employed total":"Underutilised total",
                                     names_to = "indicator",
                                     values_to = "value",
                                     values_drop_na = TRUE),
               "hours_worked" = read_absdata("hours_worked"))


               out_dir_date <- paste(sep = "-",
                                     reportabs::release(data$labour_force, "year"),
                                     stringr::str_pad(as.numeric(reportabs::release(data$labour_force, 'month')), 2, 'left', '0'),
                                     reportabs::release(data$labour_force, "month"))

               out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste(sep = "-",
                                                                                    reportabs::release(data$labour_force, "year"),
                                                                                    reportabs::release(data$labour_force, "month"),
                                                                                    state)))


               out_dir <- paste0(out_dir, "/", out_dir_date)


               knit_parameters <- list(state = state,
                                       years = years,
                                       data = data,
                                       series_type = series_type)


               rmarkdown::render(input = input,
                                 output_file = out_file,
                                 output_dir = out_dir,
                                 params = knit_parameters,
                                 envir = new.env())

               file.copy(overwrite = TRUE, from = paste0(out_dir, "/", out_file, ".pdf"), to = paste0(out_dir, "/", tolower(gsub(pattern = " ", "-", x = knit_parameters$state)), ".pdf"))

               }
