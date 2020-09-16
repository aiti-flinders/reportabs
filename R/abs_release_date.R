#' Print the most recent month/year of an ABS Time Series
#'
#' @param data the abs time series data obtained via readabs
#' @param plus add or subtract a year or month (depending on parameter ym) from the
#' @param ym 'year' for the release year and 'month' for the release month.
#'
#' @return a string
#' @importFrom dplyr filter pull distinct "%>%"
#' @importFrom lubridate month year
#' @export release
#'
#' @examples \dontrun{release(labour_force)}
release <- function(data = NULL, ym = "year", plus = 0L) {

  if (ym == "year") {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::year(release) + plus

  } else if (ym == "month") {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::month(release + months(plus), abbr = FALSE, label = TRUE)

  }

  return(release)
}
