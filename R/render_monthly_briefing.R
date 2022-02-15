#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param out_dir Directory to save the rendered documents
#' @param covid Logical. TRUE (Default) for ABS data releases with no Trend series
#' @param state String. Name of state (in full). Defaults to South Australia.T
#' @param years Number. Included graphs are drawn from January of the year specified. Minimum 1978, and defaults to 2017.
#' @param hours_worked Logical. TRUE (default) to include hours worked data. Defaults to FALSE if state is Northern Territory or Australian Capital Territory
#' @param series_type String. Seasonally Adjusted
#' @param directory dir
#' @param file file
#' @param input Path to the RMarkdown file which generates the .pdf. Defaults to the monthly_briefing file included in the reportabs package
#'
#' @return Two .pdf documents
#' @export render_monthly_briefing
#'
#'
#'
render_monthly_briefing <- function(input = system.file("markdown", "monthly_briefing.Rmd", package = 'reportabs'),
                                    out_dir = "out",
                                    .hours_worked = TRUE,
                                    covid = TRUE,
                                    state = NULL,
                                    years = NULL,
                                    series_type = NULL,
                                    directory = NULL,
                                    file = NULL) {

  if (is.null(state)) {
    message("No State specified - defaulting to South Australia")
    state <- "South Australia"
  }

  if (is.null(years)) {
    message("No base year specified - defaulting to 2017")
    years <- 2017
  }

  # if (is.null(hours_worked) & !state %in% c("Northern Territory", "Australian Capital Territory")) {
  #   message("Hours worked not specified - defaulting to TRUE")
  #   hours_worked <- TRUE
  # } else if (state %in% c("Northern Territory", "Australian Capital Territory")) {
  #   message("Hours worked not available for NT/ACT - defaulting to FALSE")
  #   hours_worked <- FALSE
  # }

  if (is.null(series_type) &  covid) {
    message("Series Type not specified - defaulting to Seasonally Adjusted (due to COVID)")
    series_type <- "Seasonally Adjusted"
  } else if (is.null(series_type) & !covid) {
    message("Series Type not specified - defaulting to Trend")
  } else {
    message()
  }

  if (is.null(directory)) {

    labour_force <- aitidata::labour_force


    out_dir_date <- paste(sep = "-",
                          reportabs::release(labour_force, "year"),
                          stringr::str_pad(as.numeric(reportabs::release(labour_force, 'month')), 2, 'left', '0'),
                          reportabs::release(labour_force, "month"))

    out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste(sep = "-",
                                                                         reportabs::release(labour_force, "year"),
                                                                         reportabs::release(labour_force, "month"),
                                                                         switch(covid + 1, NULL, "covid"),
                                                                         state)))
  } else {

    read_data <- get(load(file.path(directory, file)))

    out_dir_date <- paste(sep = "-",
                          reportabs::release(read_data, "year"),
                          stringr::str_pad(as.numeric(reportabs::release(read_data, 'month')), 2, 'left', '0'),
                          reportabs::release(read_data, "month"))

    out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste(sep = "-",
                                                                         reportabs::release(read_data, "year"),
                                                                         reportabs::release(read_data, "month"),
                                                                         switch(covid + 1, NULL, "covid"),
                                                                         state)))
  }

  out_dir <- paste0(out_dir, "/", out_dir_date)

  knit_parameters <- list(state = state,
                          years = years,
                          run = .hours_worked,
                          series_type = series_type,
                          directory = directory,
                          file = file)


  rmarkdown::render(input = input,
                    output_file = out_file,
                    output_dir = out_dir,
                    params = knit_parameters,
                    envir = new.env())

  file.copy(overwrite = TRUE, from = paste0(out_dir, "/", out_file, ".pdf"), to = paste0(out_dir, "/", tolower(gsub(pattern = " ", "-", x = knit_parameters$state)), ".pdf"))

}
