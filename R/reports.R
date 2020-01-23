#' Title
#'
#' @param data
#' @param filter_with
#'
#' @return
#'
#' @importFrom lubridate year today
#' @importFrom dplyr filter
#' @export
#'
#' @examples
last_year <- function(data, filter_with) {
  if(!is.list(filter_with)) {
    stop("Function 'last_year' requires a list!!")
  }

  if(is.null(filter_with$gender)) {
    filter_with$gender = "Persons"
  }

  if(is.null(filter_with$state)) {
    filter_with$state = "Australia"
  }

  if(is.null(filter_with$series_type) & any(data$series_type == "Trend")) {
    filter_with$series_type = "Trend"
  } else {
    filter_with$series_type = "Original"
  }
  value_last_year <- data %>%
    dplyr::filter(month == release_month(data, plus = 0),
           year %in% c(lubridate::year(lubridate::today()),
                       lubridate::year(lubridate::today())-1),
           indicator == filter_with['indicator']) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .} %>%
    dplyr::arrange(date)

  return(value_last_year)
}
