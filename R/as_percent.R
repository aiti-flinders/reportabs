#' Title
#'
#' @param string Value to print
#' @param scale Scale the value by a multiple
#' @param ... Other arguments passed to formatC
#'
#' @return character
#' @export as_percent
#'
#' @examples as_percent(50) = "50%"
as_percent <- function(string, scale = 1, ...) {

  string_format <- formatC(string*scale, format = "f", digits = 1, ...)
  string_as_percent <- stringr::str_c(string_format, "%")

  return(string_as_percent)


}
