








#' Convenience function to draw a bar chart of employment growth, by different types
#'
#' @param states Which state to plot
#' @param series_types Which series type to plot
#' @param since Growth of employment since a given year
#'
#' @return ggplot2 object
#'
employment_growth <- function(states, series_types,  since = 2010) {
  plot_data <- labour_force %>%
    dplyr::filter(
      indicator %in% c("Employed total", "Employed full-time", "Employed part-time"),
      state == states,
      age == "Total (age)",
      series_type == series_types,
      gender == "Persons"
    ) %>%
    dplyr::group_by(year, indicator) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(value = (value - dplyr::lag(value)) / value) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= years)

  p_emp_growth <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.factor(year),
      y = value,
      fill = indicator
    )) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      x = NULL,
      subtitle = "Average Annual Growth (%)",
      title = stringr::str_c("EMPLOYMENT GROWTH: ", stringr::str_to_upper(states)),
      caption = stringr::str_c(
        "Source: 6202.0 - Labour Force, Australia, ",
        release(labour_force, 'month'),
        " ",
        release(labour_force, 'year'),
        " (Table 12, ",
        series_types,
        ")"
      )
    ) +
    aititheme::theme_aiti(legend = 'bottom')

  return(p_emp_growth)

}

#' Draw employment by industry bar chart, coloured by underemployment levels. This function takes no inputs
#'
#' @return ggplot2 object
#' @export underemployment_industry
#'
#' @examples underemployment_industry()
underemployment_industry <- function() {
  data <- reportabs::employment_industry %>%
    dplyr::filter(
      indicator %in% c(
        "Underemployment ratio (proportion of employed)",
        "Employed total"
      ),
      state == "Australia",
      gender == "Persons",
      age == "Total (age)",
      industry != "Total (industry)"
    ) %>%
    dplyr::group_by(year, industry, indicator) %>%
    dplyr::summarise(value = mean(value) / 1e6) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::filter(year == max(.$year))

  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = reorder(industry, `Employed total`),
        y = `Employed total`,
        fill = `Underemployment ratio (proportion of employed)`
      )
    ) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::coord_flip() +
    ggplot2::labs(subtitle = "Total Employment (Millions)",
                  x = NULL) +
    aititheme::theme_aiti(legend = 'none')
  return(plot)

}



