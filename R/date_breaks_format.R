#' Title
#'
#' @param years
#'
#' @return
#' @export
#'
#' @examples
date_breaks_format <- function(years) {
  switch(as.character(years),
    '1' = "3 months",
    '3' = "4 months",
    '5' = "6 months",
    '10' = "1 year",
    '20' = "2 years")
}
