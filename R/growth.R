#' Title
#'
#' @param data A dataframe of tidyed ABS data
#' @param filter_with A list of which variables to filter the dataframe by. Must be a list. Can specify indicator, gender, state, age, and series_type. Indicator must be specified
#' @param year_since Baseline growth year
#'
#' @return dataframe
#' @export growth
#'
#' @examples growth(underutilisation, filter_with = list(indicator = "Unemployment rate"), year_since = 2010)
growth <- function(data = .data, filter_with = filter_list, year_since = 2010) {

  if(!is.list(filter_with)) {
    stop("Function requires a list!")
  }

  if(is.null(filter_with$indicator)) {
    stop("filter_with requires an indicator")
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

  if(is.null(filter_with$series_type) & any(data$series_type == "Trend")) {
    filter_with$series_type = "Trend"
  } else {
    filter_with$series_type = "Original"
  }

  growth_over <- data %>%
    dplyr::filter(
      indicator == filter_with['indicator']
    ) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., age == filter_with['age']) else .} %>%
    dplyr::group_by(year, indicator) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::filter(year == lubridate::year(lubridate::today()) | year == year_since) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pct = 100*(value-lag(value))/value,
      num = value-lag(value)) %>%
    dplyr::filter(!is.na(num)) %>%
    dplyr::select(num, pct)

  return(growth_over)


}
