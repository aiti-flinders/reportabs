#' Render AITI Monthly Labour Force Briefing in .pdf format
#'
#' @param out_dir Directory to save the rendered documents
#' @param state String. Name of state (in full). Defaults to South Australia.T
#' @param input Path to the RMarkdown file which generates the .pdf.
#' Defaults to the monthly_briefing file included in the reportabs package.
#'
#' @return One .pdf document
#' @export render_monthly_briefing
#'
#'
#'
render_monthly_briefing <- function(out_dir = "out",
                                    input = system.file("quarto", "monthly_briefing.qmd", package = 'reportabs'),
                                    state = "South Australia") {

  if (!rlang::is_installed("strayr") || !rlang::is_installed("ragg")) {
    stop("In order to render the monthly briefing document, the following packages must be installed:")
  }

  data <- labour_force_briefing


  out_dir_date <- paste(sep = "-",
                        reportabs::release(data, "year"),
                        stringr::str_pad(as.numeric(reportabs::release(data, 'month')), 2, 'left', '0'),
                        reportabs::release(data, "month"))

  out_file <- tolower(gsub(pattern = " ", replacement = "-", x = paste0(paste(sep = "-",
                                                                       reportabs::release(data, "year"),
                                                                       reportabs::release(data, "month"),
                                                                       state), ".pdf")))


  out_dir <- paste0(out_dir, "/", out_dir_date)


  quarto::quarto_render(input = input,
                        output_file = out_file,
                        execute_params = list(state = state))


}
