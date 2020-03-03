#' Format a value as a pretty string, with comma separation
#'
#' @param string Value to print
#' @param group A group over which to apply as_comma()
#' @param value The values to apply as_comma() to. Is NULL if group is NULL
#' @param suffix A string to print after the number
#' @param ... Additional options passed to formatC
#'
#' @return character
#' @export as_comma
#'
#' @examples
as_comma <- function(string,  group = NULL, value = NULL, suffix = NULL, ...) {

  suffix <- as.character(suffix)

  if(is.null(group) & !is.null(value)) {
    stop("Value can only be used within a group context")
  }

  if(is.null(group) & !is.data.frame(string)) {
    #Up until 1,000,000 report in thousands, ie 900,000 then reduce to millions ie 1.1 million

    if(all(round(string/1e6,1) < 1)) {

      string_format <- formatC(string, digits = 1, format = "fg", big.mark = ',', ...)
      string_as_comma <- stringr::str_c(string_format, suffix, sep = "")

    } else {

      string_format <- formatC(string/1e6, digits = 2, format = "f", ...)
      string_as_comma <- stringr::str_c(string_format, "million", suffix, sep = " ")

    }

    return(string_as_comma)

  } else if(!is.null(group) & is.data.frame(string)) {

    groups <- string %>%
      dplyr::pull(group) %>%
      unique()

    string_as_comma_group <- vector() #initialise a blank vector to fill with each groups formatted value

    for(i in groups) {

      string_value <- string %>%
        dplyr::filter(!!as.name(group) == i) %>%
        dplyr::pull(value)

      if(round(string_value/1e6, 1) < 1) {

        string_format <- formatC(string_value, digits = 1, format = "fg", big.mark = ",",  ...)
        string_as_comma <- stringr::str_c(string_format, suffix, sep = " ")
        string_as_comma_group <- append(string_as_comma_group, string_as_comma)

      } else {

        string_format <- formatC(string_value/1e6, digits = 2, format = "f", big.mark = ",", ...)
        string_as_comma <- stringr::str_c(string_format, "million", suffix, sep = " ")
        string_as_comma_group <- append(string_as_comma_group, string_as_comma)
      }
    }

    return(string_as_comma_group)
  }

}

#' Format a value as a pretty string, as a percentage
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
