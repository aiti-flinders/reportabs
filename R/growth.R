#' Title
#'
#' @param data A dataframe of tidyed ABS data
#' @param filter_with A list of which variables to filter the dataframe by. Must be a list. Can specify indicator, gender, state, age, and series_type. Indicator must be specified
#' @param year_since Baseline growth year
#'
#' @return dataframe
#' @export growth
#'
#' @examples growth(underutilisation, filter_with = list(indicator = "Unemployment rate"))
#'
growth <- function(
  data = .data,
  filter_with = filter_list,
  ym = 'year'
) {

  filtered_data <- data %>%
    filter_with(filter_with)

  units <- unique(filtered_data$unit)

  if(ym == "year") {
    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, 'month')), 1)
    value_2 <- round(value_at(data, filter_with, at_year = release(data, "year")-1, at_month = release(data, 'month')),1)
  } else if(ym == "month") {
    #You can't simply remove a month and keep the year the same. If release(month) = January, need to also subtract from year.
    if(release(data, "month") == "January") {at_year <- release(data, "year") -1 } else {at_year <- release(data, "year")}

    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, "month")),1)
    value_2 <- round(value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -1L)), 1)
  } else if (is.numeric(ym)) {

    year_adjust <- ym - release(data, "year")
    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, "month")),1)
    value_2 <- round(value_at(data, filter_with, at_year = release(data, "year", year_adjust), at_month = release(data, "month")),1)
  }

  if(units == "000") {
    value_growth <- as_comma(abs(value_1-value_2))
    percent_growth <- as_percent(100*(value_1-value_2)/value_1)
    value <- as_comma(value_1)
  } else {
    value_growth <- as_percentage_point(abs(value_1-value_2))
    percent_growth <- NULL
    value <- as_percent(value_1)
  }

  if(!is.null(percent_growth)) {
    print_string_inc <- paste0("increased by ", value_growth, " (", percent_growth, ")")
    print_string_dec <- paste0("decreased by ", value_growth, " (", percent_growth, ")")
    print_string_c <- paste0("remained steady at ", value)
  } else {
    print_string_inc <- paste0("increased by ", value_growth)
    print_string_dec <- paste0("decreased by ", value_growth)
    print_string_c <- paste0("remained steady at ", value)
  }

  dplyr::case_when(value_1 > value_2 ~ print_string_inc,
    value_1 < value_2 ~ print_string_dec,
    value_1 == value_2 ~ print_string_c)
}
