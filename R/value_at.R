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
value_at <- function(data = ., filter_with = NULL, month_adjust = 0, at_year, at_month) {

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


  value_at_time <- data %>%
    dplyr::filter(
      month == at_month,
      year == at_year,
      indicator == filter_with['indicator']
    ) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., age == filter_with['age']) else .} %>%
    pull(value)

  return(value_at_time)

}
