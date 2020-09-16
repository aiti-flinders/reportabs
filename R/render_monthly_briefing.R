#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param input Path to the RMarkdown file which generates the .pdf. Defaults to the monthly_briefing file included in the reportabs package
#' @param output_dir Directory to output the file
#' @param output_file_name Name of output file
#' @param params Parameters passed to .Rmd file. The included .Rmd takes parameters of state (string), years (num), run (logical), and series_type (string)
#'
#' @return Two .pdf documents
#' @export render_monthly_briefing
#'
render_monthly_briefing <- function(input = system.file("doc", "monthly_briefing.Rmd", package = 'reportabs'),
                                    out_dir = "out",
                                    covid = TRUE,
                                    state = NULL,
                                    years = NULL,
                                    hours_worked = NULL,
                                    series_type = NULL) {

  if (is.null(state)) {
    message("No State specified - defaulting to South Australia")
    state <- "South Australia"
  }

  if (is.null(years)) {
    message("No base year specified - defaulting to 2017")
    years <- 2017
  }

  if (is.null(hours_worked)) {
    message("Hours worked not specified - defaulting to TRUE")
    hours_worked <- TRUE
  } else if (state %in% c("Northern Territory", "Australian Capital Territory")) {
    message("")
    hours_worked <- FALSE
  }

  if (is.null(series_type)) {
    message("Series Type not specified - defaulting to Seasonally Adjusted")
    series_type <- "Seasonally Adjusted"
  }
  abs_date <- daitir::abs_current_release("6202.0")
  out_dir_date <- paste(sep = "-",
                        lubridate::year(abs_date),
                        stringr::str_pad(lubridate::month(abs_date), 2, 'left', '0'),
                        lubridate::month(abs_date, abbr = FALSE, label = TRUE))

  out_dir <- paste0(out_dir, "/", out_dir_date)

  knit_parameters <- list(state = state, years = years, run = hours_worked, series_type = series_type)

  out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste(sep = "-",
                    lubridate::year(abs_date),
                    lubridate::month(abs_date, abbr = FALSE, label = TRUE),
                    ifelse(covid, "covid", NULL),
                    knit_parameters$state)))


  rmarkdown::render(input = input,
                    output_file = out_file,
                    output_dir = out_dir,
                    params = knit_parameters)

  file.copy(from = paste0(out_dir, "/", out_file, ".pdf"), to = paste0(out_dir, "/", tolower(gsub(pattern = " ", "-", x = knit_parameters$state)), ".pdf"))

}
