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
#' @importFrom zoo rollapplyr
#' @importFrom rlang .data
#'
#'
#'@examples \dontrun{sahm("Australia")}
#'
sahm <- function(region = "Australia") {

  data <- read_absdata("labour_force") %>%
    dplyr::filter(.data$indicator == "Unemployment rate",
      .data$gender == "Persons",
      .data$age == "Total (age)",
      .data$series_type == "Seasonally Adjusted") %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(value_3mo = zoo::rollapplyr(FUN = "mean", .data$value, width = 3, fill = NA),
      value_12_mo_min = zoo::rollapplyr(FUN = "min", .data$value, width = 13, fill = NA),
      sahm = .data$value_3mo - .data$value_12_mo_min) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(sahm))

  if(length(region) < 2) {

    data %>%
      dplyr::filter(.data$state == region) %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), colour = aititheme::aiti_darkblue) +
      ggplot2::labs(
        x = NULL,
        title = paste0("Sahm Rule Recession Indicator: ", region)
      ) +
      aititheme::theme_aiti()
  }

  else {

    data %>%
      dplyr::filter(.data$state %in% region) %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = sahm)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5),colour = aititheme::aiti_darkblue) +
      ggplot2::facet_wrap(state~.) +
      ggplot2::labs(
        x = NULL
      ) +
      aititheme::theme_aiti()
  }
}
