#' Title
#'
#' @param data
#' @param plus
#'
#' @return
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#' @importFrom lubridate month
#' @export
#'
#' @examples
release_month <- function(data, plus = 1L) {
  rel_m <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::pull(date) %>%
    max() %>%
    `+`(months(plus)) %>%
    lubridate::month(abbr = FALSE, label = TRUE)

  return(rel_m)
}
