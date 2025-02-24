#' Plot the Sahm Recession Indicator for a given state. The Sahm indicator compares the 3 month
#' average (seasonally adjusted) unemployment rate with the minimum over the last 12 months.
#' Sahm values over 1 are indicative
#' of a recession.
#'
#' @param state The state or territory to plot the indicator for. Accepts Australia
#' @param data A dataframe with at least the columns date, state, and value
#'
#' @return a ggplot2 object
#' @export sahm
#'
#' @importFrom rlang .data
#'
#'
#'@examples \dontrun{sahm("Australia")}
#'
sahm <- function(data, state = "Australia") {

  if (is.null(data)) {
    data <- read_absdata("labour_force")
  }

  sahm_data <- data |>
    filter_list(over = list(indicator = "Unemployment rate",
                            series_type = "Seasonally Adjusted",
                            sex = "Persons",
                            age = "Total (age)",
                            state = {{state}})) |>
    dplyr::group_by(.data$state) |>
    dplyr::mutate(value_3mo = zoo::rollmeanr(.data$value, k = 3, fill = NA),
                  value_12_mo_min = zoo::rollapplyr(FUN = "min", .data$value_3mo, width = list(-(1:12)), fill = NA),
      sahm = .data$value_3mo - .data$value_12_mo_min) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(sahm))

  if(length(state) < 2) {

    sahm_data |>
      dplyr::filter(.data$state == state) |>
      ggplot2::ggplot(ggplot2::aes(x = date, y = sahm, col = state)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), colour = fof_colours["Summer Red"]) +
      ggplot2::labs(
        x = NULL,
        y = "percentage points",
        title = state
      ) +
      theme_fof() +
      scale_colour_fof(reverse = T)
  }

  else {

    sahm_data |>
      dplyr::filter(.data$state %in% state) |>
      ggplot2::ggplot(ggplot2::aes(x = date, y = sahm, col = state)) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0.5), colour = fof_colours["Summer Red"]) +
      ggplot2::facet_wrap(state~.) +
      ggplot2::labs(
        x = NULL,
        y = "percentage points"
      ) +
      theme_fof() +
      scale_colour_fof()
  }
}

recessions <- function() {

}
