#' Title
#'
#' @param state
#' @param coord_limits
#'
#' @return
#' @export
#'
#' @examples
sahm <- function(region = "Australia", coord_limits = NULL) {

  data <- reportabs::underutilisation %>%
    dplyr::filter(indicator == "Unemployment rate",
      gender == "Persons",
      age == "Total (age)",
      series_type == "Seasonally Adjusted") %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value_3mo = zoo::rollmean(value, 3, align = 'right', na.pad = TRUE),
      value_12_mo_min = -zoo::rollmax(-value, 12, align = 'right', na.pad = TRUE),
      sahm = value_3mo - value_12_mo_min) %>%
    dplyr::ungroup()

  if(length(region) < 2) {

    data <- data %>%
      dplyr::filter(state == region)

    ggplot2::ggplot(data, ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0))
  }

  else {

    data <- data %>%
      dplyr::filter(state %in% region)

    ggplot2::ggplot(data, ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
      ggplot2::facet_wrap(state~.)
  }
}
