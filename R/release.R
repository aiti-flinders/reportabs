#' Title
#'
#' @param data
#' @param plus
#'
#' @return
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#' @importFrom lubridate month
#' @export release
#'
#' @examples
release <- function(data = .data, ym = 'year', plus = 0L) {

  if (ym == 'year') {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::year(release) + plus

  } else if (ym == 'month') {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::month(release + months(plus), abbr = FALSE, label = TRUE)

  }

  return(release)
}
