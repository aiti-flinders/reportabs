#' Employment Time Series
#'
#' @param states which states to include in the plot
#' @param dbf how many years does the data span? this adjsuts the x axis scale accordingly
#'
#' @return a ggplot2 object
#' @export abs_employment
#'
#' @examples
abs_employment <- function(states,  years, ages = "Total (age)", genders = "Persons", series_types = "Trend") {
  plot_data <- labour_force %>%
    dplyr::filter(indicator == "Employed total",
      gender %in% genders,
      series_type == series_types,
      state %in% states,
      year >= max(.$year) - years) %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(index = 100*value/value[1])

  plot_month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = index, colour = state)) +
    ggplot2::geom_line() +

    ggplot2::labs(x = NULL,
      y = paste("Index (Base:", plot_month, plot_year, "=100)"),
      title = paste("EMPLOYMENT: ", stringr::str_to_upper(strayr::strayr(states[1])), " & AUSTRALIA"),
      caption = "Source: ABS Labour Force Survey (6202.0, Table 12, Trend)") +
    ggplot2::scale_x_date(date_breaks = date_breaks_format(years), labels = scales::date_format("%b-%y")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'bottom',
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = 'black'))

  return(plot)

}

#' Unemployment (level) Time Series
#'
#' @param states
#' @param dbf
#'
#' @return
#' @export abs_unemployment
#'
#' @examples
abs_unemployment <- function(states,  years, ages = "Total (age)", genders = "Persons", series_types = "Trend") {
  plot_data <- labour_force %>%
    dplyr::filter(indicator == "Unemployed total",
      gender == genders,
      age == ages,
      series_type == series_types,
      state %in% states,
      year >= max(.$year) - years) %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(index = 100*value/value[1])

  plot_month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = index, colour = state)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = NULL,
      y = paste("Index (Base:", plot_month, plot_year, "=100)"),
      title = paste("UNEMPLOYMENT: ", stringr::str_to_upper(strayr::strayr(states[1])), " & AUSTRALIA"),
      caption = "Source: ABS Labour Force Survey (6202.0, Table 12, Trend)") +
    ggplot2::scale_x_date(date_breaks = date_breaks_format(years), labels = scales::date_format("%b-%y")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'bottom',
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = 'black'))


  return(plot)

}

#' ABS Unemployment Rate Time Series
#'
#' @param states
#' @param dbf
#'
#' @return
#' @export abs_unemployment_rate
#'
#' @examples
abs_unemployment_rate <- function(states,  years, ages = "Total (age)", genders = "Persons", series_types = "Trend") {
  plot_data <- labour_force %>%
    filter(indicator == "Unemployment rate",
      gender == "Persons",
      age == "Total (age)",
      series_type == "Trend",
      state %in% states,
      year >= max(.$year) - years)

  plot_month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value, colour = state)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = NULL,
      y = NULL,
      title = paste("UNEMPLOYMENT RATE: ", stringr::str_to_upper(strayr::strayr(states[1])), " & AUSTRALIA"),
      caption = "Source: ABS Labour Force Survey (6202.0, Table 12, Trend)") +
    ggplot2::scale_x_date(date_breaks = date_breaks_format(years), labels = scales::date_format("%b-%y")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'bottom',
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = 'black'))

  return(plot)
  }

#' Convenience function to draw a bar chart of employment growth, by different types
#'
#' @param data
#' @param filter_with
#' @param year_since
#'
#' @return
#' @export employment_growth
#'
#' @examples
employment_growth <- function(data = NULL, filter_with = list(), year_since = 2010) {

  if(is.null(data)) {data <- reportabs::underutilisation}

  cols <- c("Employed full-time" = "#041457", "Employed part-time" = "#006eff", "Employed total" = "#a1a1a1")

  if(!is.list(filter_with)) {
    stop("Function requires a list!")
  }

  if(is.null(filter_with$gender)) {
    filter_with$gender = "Persons"
  }

  if(is.null(filter_with$state)) {
    filter_with$state = "Australia"
  }

  if(is.null(filter_with$age)) {
    filter_with$age = "Total (age)"
  }

  if(is.null(filter_with$industry)) {
    filter_with$industry = "Total all industries"
  }


  if(is.null(filter_with$series_type) & any(data$series_type == "Trend")) {
    filter_with$series_type = "Trend"
  } else {
    filter_with$series_type = "Original"
  }


  p_emp_growth <- data %>%
    dplyr::filter(indicator %in% c("Employed total", "Employed part-time", "Employed full-time")) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., age == filter_with['age']) else .} %>%
    {if("industry" %in% names(.)) dplyr::filter(., industry == filter_with['industry']) else .} %>%
    dplyr::group_by(year, indicator) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(value = (value - dplyr::lag(value))/value) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= year_since) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(year), y = value, fill = indicator)) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_fill_manual(values = cols, labels = c("Full Time", "Part Time", "Total"), name = "Employment Type") +
    ggplot2::labs(x = NULL,
      y = "Average Annual Growth (%)",
      caption = stringr::str_c("Source: ABS 6202.0, ", format(max(data$date), "%B %Y")))

  return(p_emp_growth)

}
