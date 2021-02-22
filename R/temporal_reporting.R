#' Value of an ABS Time Series indicator for a specific year and month.
#'
#' @param data a dataframe of cleaned ABS Time Series data returned from readabs
#' @param filter_with a list of variables to filter the dataframe on. Valid variables include
#' gender, age, indicator, and series type.
#' @param at_year a year (numeric). Defaults to the most recent year if NULL (the default) and at_month = NULL.
#' If only at_year is specified, the value is averaged over the year (Not yet implemented)
#' @param at_month a month(name). Defaults to the most recent month if NULL (the default)
#'
#' @return a number
#' @export value_at
#'
#' @examples \dontrun{value_at(labour_force, filter_with = list(indicator = "Employed total"))}
#'
value_at <- function(data = NULL, filter_with = filter_list,  at_year = NULL, at_month = NULL) {
  if(is.null(at_year) & is.null(at_month)) {
    at_year <- release(data, 'year')
    at_month <- release(data, 'month')
  }

  if(is.null(at_month)) {at_month <- release(data, "month")}

  filtered_data <- data %>%
    filter_with(filter_with)

  value_at_time <- filtered_data %>%
    dplyr::filter(
      month == at_month,
      year == at_year)

  #Sometimes, like with employment by industry, there is no single value for a specified time. In this case we should return the whole data frame
  if(nrow(value_at_time)>1) {
    value_at_time <- value_at_time
  } else {value_at_time <- value_at_time$value}

  return(value_at_time)

}


#' Find the value for an indicator for this month last year, or the previous month.
#'
#' @param data a dataframe of cleaned ABS Time Series data returned from readabs
#' @param filter_with a list of variables to filter the dataframe on. Valid variables include
#' gender, age, indicator, and series type.
#' @param ym one of "year", "quarter", or "month"  to return the value this time last year (the default), quarter, or month.
#' @param print logical to pass the numeric on to as_comma or as_percent for printing. Defaults to TRUE
#'
#' @return numeric if print == FALSE, character if print == TRUE
#'
#' @export last_value
#'
#' @examples \dontrun{last_value(labour_force, filter_with = list(indicator = "Employed total"))}
#'


last_value <- function(data = NULL, filter_with = filter_list, ym = 'year', print = TRUE) {

  filtered_data <- data %>%
    filter_with(filter_with)

  units <- unique(filtered_data$unit)

  if(ym == "year") {

    value_last <- value_at(data, filter_with, at_year = release(data, 'year', -1), at_month = release(data, 'month'))

  } else if(ym == "month") {

    if(release(data, "month") == "January") {
      at_year <- release(data, "year", -1)
    } else {
      at_year <- release(data, "year")
    }
    value_last <- value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -1))

  } else if (ym == "quarter") {

    if (release(data, "month") %in% c("January", "February", "March")) {
      at_year <- release(data, "year", -1)
    } else {
      at_year <- release(data, "year")
    }

    value_last <- value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -3))
  }

  if(!print) {
    value_last <- value_last
  } else if(units == "000") {
    value_last <- as_comma(value_last)
  } else {
    value_last <- as_percent(value_last)
  }

  return(value_last)


}

#' Return the most recent (current) value for an indicator
#'
#' @param data a dataframe of cleaned ABS Time Series data returned from readabs
#' @param filter_with a list of variables to filter the dataframe on. Valid variables include
#' gender, age, indicator, and series type.
#' @param print logical. If TRUE (default) current() returns a string. Specify FALSE to return a number.
#'
#' @return String if print == TRUE. Number if print == FALSE
#' @export
#'
#' @examples \dontrun{current(labour_force, list(indicator = "Employed total"))}
#'
current <- function(data = NULL, filter_with = filter_list, print = TRUE) {

  filtered_data <- data %>%
    filter_with(filter_with) %>%
    filter(date == max(date))

  units <- unique(filtered_data$unit)

  if(nrow(filtered_data)>1) {
    print <- TRUE
    filtered_data <- filtered_data
  } else if(!print) {

    filtered_data <- filtered_data$value

  } else if (units == "000") {

    filtered_data <- as_comma(filtered_data$value)

  } else if(units != "000") {

    filtered_data <- as_percent(filtered_data$value)

  }


  return(filtered_data)

}


#' Create a string describing whether an indicator has increased or decreased over the year,
#' or over the previous month
#'
#' @param data a dataframe of cleaned ABS Time Series data returned from readabs
#' @param filter_with a list of variables to filter the dataframe on. Valid variables include
#' gender, age, indicator, and series type.
#' @param type controls the wording of the text.
#' Options are 'id' for increased or decreased, "ab" for above or below, "rf" for risen or fallen and "present"
#' for an increase or a decrease.
#' @param ym ym = "year" to calculate the change over the year, or ym = "month" to calculate the change over the month
#' @param at_year By default, change() returns the difference over the past 12 months (to the current year). at_year and at_month can
#' be specified to calculate the change between the current value, and the value as at at_year and at_month.
#' @param at_month By default, change() returns the difference over the past 12 months (to the current year). at_year and at_month can
#' be specified to calculate the change between the current value, and the value as at at_year and at_month.
#'
#' @return character
#' @export change
#'
#' @examples \dontrun{change(labour_force, filter_with = list(indicator = "Employed total"))}
change <- function(
  data = NULL,
  filter_with = filter_list,
  type = 'id',
  ym = 'year',
  at_year = NULL,
  at_month = NULL
) {

  filtered_data <- data %>%
    filter_with(filter_with)

  units <- unique(filtered_data$unit)
  if(!is.null(at_year) | !is.null(at_month)) {
    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, 'month')), 1)
    value_2 <- round(value_at(data, filter_with, at_year = at_year, at_month = at_month),1)
  } else  if(ym == "year") {
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
    value_change <- as_percentage_point(abs(value_1-value_2))
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

#' The average of an indicator between two years
#'
#' @param data a dataframe of cleaned ABS Time Series data returned from readabs
#' @param filter_with a list of variables to filter the dataframe on. Valid variables include
#' gender, age, indicator, and series type.
#' @param between a character vector of the first and last years (inclusive) to average over
#'
#' @return numeric
#' @export average_over
#'
#' @examples \dontrun{average_over(labour_force, list(indicator = "Employed total"), between = c(2010,2015))}
#'
average_over <- function(data = NULL, filter_with = filter_list, between) {
  filtered_data <- data %>%
    filter_with(filter_with)

  average_over <- filtered_data %>%
    dplyr::filter(year >= min(between),
      year <= max(between)) %>%
    dplyr::summarise(value = mean(value)) %>%
    pull(value)

  return(average_over)


}
