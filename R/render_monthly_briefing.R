#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param state String. Name of state (in full). Defaults to South Australia.
#' @param output_format Format to render the document. One of "html" or "pdf"
#'
#' @export render_monthly_briefing
#'

render_monthly_briefing <- function(state = "South Australia",
                                    output_format) {

  if (!rlang::is_installed("strayr") || !rlang::is_installed("ragg")) {
    stop("In order to render the monthly briefing document, the following packages must be installed:")
  }

  labour_force_briefing <- read_absdata("labour_force_briefing")


  out_dir_date <- paste(sep = "-",
                        release(labour_force_briefing, "year"),
                        stringr::str_pad(as.numeric(reportabs::release(labour_force_briefing, 'month')), 2, 'left', '0'),
                        release(labour_force_briefing, "month"))

  out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste0(paste(sep = "-",
                                                                       release(labour_force_briefing, "year"),
                                                                       release(labour_force_briefing, "month"),
                                                                       state), ".", output_format)))


  out_dir <- paste0("out", "/", out_dir_date)


  quarto::quarto_render(input = system.file("quarto", "monthly_briefing.qmd", package = "reportabs"),
                        output_file = out_file,
                        output_format = output_format,
                        execute_params = list(state = state))


}

