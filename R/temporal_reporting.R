#' Value at a specific year/month
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
value_at <- function(data = .data, filter_with = NULL, month_adjust = 0, at_year, at_month) {

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


last <- function(data =.data, filter_with = NULL, ym = 'year') {

  if(ym == "year") {

    value_last_year <- value_at(data, filter_with, at_year = release(data, 'year', -1), at_month = release(data, 'month'))

    return(value_last_year)

  } else if(ym == "month") {
    if(release(data, "month") == "January") {at_year <- release(data, "year", -1)} else {at_year <- release(data, "year")}
    value_last_month <- value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -1))
    return(value_last_month)


  }


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
current <- function(data, filter_with = NULL) {

  filtered_data <- data %>%
    filter_with(filter_with) %>%
    filter(date == max(date)) %>%
    pull(value)



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
  filter_with = NULL,
  type = 'id',
  ym = 'year'
) {

  filtered_data <- data %>%
    filter_with(filter_with)

  units <- filtered_data %>%
    pull(unit) %>%
    unique()

  if(ym == "year") {
    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, 'month')), 1)
    value_2 <- round(value_at(data, filter_with, at_year = release(data, "year")-1, at_month = release(data, 'month')),1)
  } else if(ym == "month") {
    #You can't simply remove a month and keep the year the same. If release(month) = January, need to also subtract from year.
    if(release(data, "month") == "January") {at_year <- release(data, "year") -1 } else {at_year <- release(data, "year")}

    value_1 <- round(value_at(data, filter_with, at_year = release(data, "year"), at_month = release(data, "month")),1)
    value_2 <- round(value_at(data, filter_with, at_year = at_year, at_month = release(data, "month", -1L)), 1)
  }

  if(units == "000") {
    value_change <- as_comma(abs(value_1-value_2))
    value <- as_comma(value_1)
  } else {
    value_change <- as_percent(abs(value_1-value_2))
    value <- as_percent(value_1)}

  if(type == "id") {
    dplyr::case_when(value_1 > value_2 ~ stringr::str_c('increased by ', value_change, " to ", value),
      value_1 < value_2 ~ stringr::str_c('decreased by ', value_change, " to ", value),
      value_1 == value_2 ~ stringr::str_c("remained steady at", value_1))
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
