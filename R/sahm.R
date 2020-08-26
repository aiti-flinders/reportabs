#' Plot the Sahm Recession Indicator for a given state. The Sahm indicator compares the 3 month
#' average (seasonally adjusted) unemployment rate with the minimum over the last 12 months.
#' Sahm values over 1 are indicative
#' of a recession.
#'
#' @param region The state or territory to plot the indicator for. Accepts Australia
#'
#' @return a ggplot2 object
#' @export sahm
#'
#' @import ggplot2
#' @import zoo
#'
#'
#'@examples \dontrun{sahm("Australia")}
#'
sahm <- function(region = "Australia") {

  data <- daitir::labour_force %>%
    dplyr::filter(indicator == "Unemployment rate",
      gender == "Persons",
      age == "Total (age)",
      series_type == "Seasonally Adjusted") %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(value_3mo = zoo::rollmeanr(value, 3,  fill = NA),
      value_12_mo_min = zoo::rollapplyr(FUN = "min", value, width = 13, fill = NA),
      sahm = value_3mo - value_12_mo_min) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(sahm))

  if(length(region) < 2) {

    data <- data %>%
      dplyr::filter(state == region)

    ggplot2::ggplot(data, ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), colour = aititheme::aiti_darkblue) +
      ggplot2::labs(
        x = NULL,
        title = paste0("Sahm Rule Recession Indicator: ", region)
      ) +
      aititheme::theme_aiti()
  }

  else {

    data <- data %>%
      dplyr::filter(state %in% region)

    ggplot2::ggplot(data, ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5),colour = aititheme::aiti_darkblue) +
      ggplot2::facet_wrap(state~.) +
      aititheme::theme_aiti()
  }
}
