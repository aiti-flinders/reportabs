#' Value of an ABS Time Series indicator for a specific year and month.
#'
#' @param data a dataframe
#' @param filter_with a list of variables to filter the dataframe on
#' @param at_year a year (numeric)
#' @param at_month a month(name)
#' @param month_adjust
#'
#' @return
#' @export value_at
#'
#' @examples
value_at <- function(data = .data, filter_with = filter_list, month_adjust = 0, at_year = NULL, at_month = NULL) {
  if(is.null(at_year)) {at_year <- release(data, 'year')}
  if(is.null(at_month)) {at_month <- release(data, "month")}

  filtered_data <- data %>%
    filter_with(filter_with)

  value_at_time <- filtered_data %>%
    dplyr::filter(
      month == at_month,
      year == at_year) %>%
    pull(value)

  return(value_at_time)

}


#' Find the value for an indicator for this month last year, or the previous month.
#'
#' @param data
#' @param filter_with
#'
#' @return
#'
#' @importFrom lubridate year today
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export last
#'
#' @examples
#'


last_value <- function(data = .data, filter_with = filter_list, ym = 'year') {

  filtered_data <- data %>%
    filter_with(filter_with)

  units <- unique(filtered_data$unit)

  if(ym == "year") {

    value_last <- value_at(data, filter_with, at_year = release(data, 'year', -1), at_month = release(data, 'month'))

  } else if(ym == "month") {

    if(release(data, "month") == "January") {at_year <- release(data, "year", -1)} else {at_year <- release(data, "year")}

    value_last <- value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -1))

  }

  if(units == "000") {value_last <- as_comma(value_last)} else {value_last <- as_percent(value_last)}

  return(value_last)


}

#' Return the most recent value for an indicator
#'
#' @param data
#' @param filter_with
#'
#' @return num
#' @export current
#' @importFrom magrittr "%>%"
#'
#' @examples
current <- function(data = .data, filter_with = filter_list) {

  filtered_data <- data %>%
    filter_with(filter_with) %>%
    filter(date == max(date))

  units <- unique(filtered_data$unit)

  if (units == "000") {
    filtered_data <- as_comma(filtered_data$value)

  } else {
    filtered_data <- as_percent(filtered_data$value)

  }



  return(filtered_data)

}


#' Report whether an indicator has increased or decreased over the year, or over the previous month
#'
#' @param data
#' @param period
#' @param type
#' @param ym
#'
#' @return string
#' @export change
#'
#' @examples
change <- function(
  data = .data,
  filter_with = filter_list,
  type = 'id',
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
  } else if(is.numeric(ym)) {

    #If neither year or month is specified, allow the input to be a year. To generate the right at_year, need to subtract from the       release year the difference between release year and input year. Ie if ym == 1980, at_year for value_2 is release(data, "year"       )-(release(data, "year")-1980)

    year_adjust <- ym - release(data, "year")

    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, "month")),1)
    value_2 <- round(value_at(data, filter_with, at_year = release(data, "year", year_adjust), at_month = release(data, "month")),1)
  }

  if(units == "000") {
    value_change <- as_comma(abs(value_1-value_2))
    percent_change <- as_percent(100*(value_1-value_2)/value_1)
    value <- as_comma(value_1)
  } else {
    value_change <- as_percent(abs(value_1-value_2))
    percent_change <- NULL
    value <- as_percent(value_1)}

  if(!is.null(percent_change)) {
    print_string_inc <- paste0("increased by ", value_change, " (", percent_change, ") to ", value)
    print_string_dec <- paste0("decreased by ", value_change, " (", percent_change, ") to ", value)
    print_string_c <- paste0("remained steady at ", value)
  } else {
    print_string_inc <- paste0("increased by ", value_change, " to ", value)
    print_string_dec <- paste0("decreased by ", value_change, " to ", value)
    print_string_c <- paste0("remained steady at ", value)
  }

  if(type == "id") {
    dplyr::case_when(value_1 > value_2 ~ print_string_inc,
      value_1 < value_2 ~ print_string_dec,
      value_1 == value_2 ~ print_string_c)
  } else if(type == "ab") {
    dplyr::case_when(value_1 > value_2 ~ 'above',
      value_1 < value_2 ~ 'below',
      value_1 == value_2 ~ 'the same as')
  } else if(type == 'rf') {
    dplyr::case_when(value_1 > value_2 ~ 'risen',
      value_1 < value_2 ~ 'fallen',
      value_1 == value_2 ~ 'remained steady at')
  } else if(type == 'present') {
    dplyr::case_when(value_1 > value_2 ~ 'an increase',
      value_1 < value_2 ~ 'a decrease',
      value_1 == value_2 ~ stringr::str_c("the same level as last", ym))
  }
}

#' Title
#'
#' @param data
#' @param filter_with
#' @param between
#'
#' @return
#' @export average_over
#'
#' @examples
#'
average_over <- function(data = .data, filter_with = filter_list, between) {
  filtered_data <- data %>%
    filter_with(filter_with)

  average_over <- filtered_data %>%
    dplyr::filter(year >= min(between),
      year <= max(between)) %>%
    dplyr::summarise(value = mean(value)) %>%
    pull(value)

  return(average_over)


}
