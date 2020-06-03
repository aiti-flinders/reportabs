#' Format date breaks for ggplots with time on the x axis, depending on how many years of data being plot.
#' This function is not exported
#'
date_breaks_format <- function(years) {
  switch(as.character(years),
    '1' = "3 months",
    '3' = "4 months",
    '5' = "6 months",
    '10' = "1 year",
    '20' = "2 years",
    '40' = "5 years")
}
