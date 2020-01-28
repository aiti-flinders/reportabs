#' Reporting functions
#'
#' @param data
#' @param filter_with
#'
#' @return
#'
#' @importFrom lubridate year today
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'


last <- function(data =., filter_with = NULL, ym = 'year') {
  if(!is.list(filter_with)) {
    stop("Function 'last_year' requires a list!")
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

  #If it's January, then today() will return a year which has no data released for it yet. Data for 2020 for example is not available until February of 2020.

  date <- lubridate::today()


  if(ym == "year") {

  if(lubridate::month(date) < 2) {
    year_filter <- c(lubridate::year(date)-1, lubridate::year(date)-2)
  } else {
    year_filter <- c(lubridate::year(date), lubridate::year(date)-1)
  }

    value_last_year <- data %>%
      dplyr::filter(
        month == release_month(data, plus = 0),
        year %in% year_filter,
        indicator == filter_with['indicator']
      ) %>%
      {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with["gender"]) else .} %>%
      {if("state" %in% names(.)) dplyr::filter(., state == filter_with["state"]) else .} %>%
      {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with["series_type"]) else .} %>%
      {if("age" %in% names(.)) dplyr::filter(., age == filter_with["age"]) else .} %>%
      dplyr::arrange(date) %>%
      dplyr::pull(value)

    value_last_year_report <- abs(
      c(value_last_year[2]-value_last_year[1], 100*(value_last_year[2]-value_last_year[1])/value_last_year[2])
    )

    names(value_last_year_report) <- c("num", "pct")

    return(value_last_year_report)

  } else if(ym == "month") {

    value_last_month <- data %>%
      dplyr::filter(
        month == release_month(data, plus = 0) | month == release_month(data, plus = -1L),
        indicator == filter_with['indicator']
      ) %>%
      {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with["gender"]) else .} %>%
      {if("state" %in% names(.)) dplyr::filter(., state == filter_with["state"]) else .} %>%
      {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with["series_type"]) else .} %>%
      {if("age" %in% names(.)) dplyr::filter(., age == filter_with["age"]) else .} %>%
      dplyr::arrange(date) %>%
      dplyr::pull(value)

    value_last_month_report <- abs(
      c(value_last_month[2]-value_last_month[1], 100*(value_last_month[2]-value_last_month[1])/value_last_month[2])
    )

    names(value_last_month_report) <- c("num", "pct")

    return(value_last_month_report)


  }


}
