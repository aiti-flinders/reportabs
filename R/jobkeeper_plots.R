#' Jobkeeper plots
#'
#' @param data
#' @param indicator
#' @param states
#' @param years
#' @param compare_aus
#' @param plotly
#' @param void
#'
#' @return
#' @export
#'
#' @examples
jobkeeper_plots <- function(data = NULL,
                            indicator,
                            states,
                            years,
                            compare_aus = TRUE,
                            plotly = FALSE,
                            void = FALSE) {

  if (compare_aus & !"Australia" %in% states) {
    states <- c(states, "Australia")
    #plot_parameters$n_cols <- plot_parameters$n_cols + 1
  }

  if (is.null(data)) {
    plot_data <- aitidata::jobkeeper_state
  }

  plot_data <- plot_data %>%
    dplyr::filter(.data$indicator == !!indicator) %>%
    dplyr::group_by(.data$state) %>%
    dplyr::mutate(index = 100 * .data$value / .data$value[1]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$state %in% states)


  xmin_date <- as.Date("2020-10-01")
  xmax_date <- as.Date(max(plot_data$date))

  col_var <- "state"

  plot_parameters <- plot_parameters(plot_data, states, indicator, compare_aus = compare_aus)
  create_plot(plot_data, plot_parameters, void = void, plotly = plotly)

}
