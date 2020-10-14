#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param input Path to the RMarkdown file which generates the .pdf. Defaults to the monthly_briefing file included in the reportabs package
#' @param output_dir Directory to output the file
#' @param output_file_name Name of output file
#'
#' @return Two .pdf documents
#' @export render_monthly_briefing
#'
#' @importFrom stringr str_c
#'
#'
render_monthly_briefing <- function(input = system.file("markdown", "monthly_briefing.Rmd", package = 'reportabs'),
                                    out_dir = "out",
                                    covid = TRUE,
                                    state = NULL,
                                    years = NULL,
                                    hours_worked = NULL,
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

  if (is.null(hours_worked) & !state %in% c("Northern Territory", "Australian Capital Territory")) {
    message("Hours worked not specified - defaulting to TRUE")
    hours_worked <- TRUE
  } else if (state %in% c("Northern Territory", "Australian Capital Territory")) {
    message("Hours worked not available for NT/ACT - defaulting to FALSE")
    hours_worked <- FALSE
  }

  if (is.null(series_type) & !state %in% c("Northern Territory", "Australian Capital Territory") & covid) {
    message("Series Type not specified - defaulting to Seasonally Adjusted (due to COVID)")
    series_type <- "Seasonally Adjusted"
  } else if (is.null(series_type) & !covid) {
    message("Series Type not specified - defaulting to Trend")
  } else if (state %in% c("Northern Territory", "Australian Capital Territory") & covid) {
    message("Trend not available for NT/ACT (due to COVID) - defaulting to Original")
    series_type <- "Original"
  } else {
    message()
  }

  if (is.null(directory)) {

    labour_force <- daitir::labour_force


    out_dir_date <- paste(sep = "-",
                          reportabs::release(labour_force, "year"),
                          stringr::str_pad(as.numeric(reportabs::release(labour_force, 'month')), 2, 'left', '0'),
                          reportabs::release(labour_force, "month"))

    out_file <- tolower(gsub(pattern = " ", replacement = "-", x = stringr::str_c(sep = "-",
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

    out_file <- tolower(gsub(pattern = " ", replacement = "-", x = stringr::str_c(sep = "-",
                                                                         reportabs::release(read_data, "year"),
                                                                         reportabs::release(read_data, "month"),
                                                                         switch(covid + 1, NULL, "covid"),
                                                                         state)))
  }

  out_dir <- paste0(out_dir, "/", out_dir_date)

  knit_parameters <- list(state = state,
                          years = years,
                          run = hours_worked,
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
