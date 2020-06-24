#' Format date breaks for ggplots with time on the x axis, depending on how many years of data being plot.
#' This function is not exported
#'
date_breaks_format <- function(years) {
  dplyr::case_when(
    years == 1 ~ "3 months",
    years <= 3 ~ "4 months",
    years <= 5 ~ "6 months",
    years <= 10 ~ "1 year",
    years <= 20 ~ "2 years",
    years <= 40 ~ "5 years",
    TRUE ~ "10 years"
  )
}
