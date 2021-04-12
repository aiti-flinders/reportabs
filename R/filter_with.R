#' Filter a dataframe with semi-consistent column names (ie some series do not have a "gender") and semi-consistent
#' variables within columns (ie some series do not have a trend indicator for some states).
#' This function is not exported
#'
#' @param data a tidy ABS time series dataframe
#' @param filter_with a list of variables to filter the dataframe by
#'
#' @importFrom rlang .data
#'

filter_with <- function(data = NULL, filter_with = NULL) {
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


  filtered_data <- data %>%
    dplyr::filter(.data$indicator %in% filter_with$indicator) %>%
    {if("gender" %in% names(.)) dplyr::filter(., .data$gender %in% filter_with$gender) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., .data$state %in% filter_with$state) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., .data$age %in% filter_with$age) else .}

  #Can only check if the indicator has a Trend series after it has been filtered.

  if(is.null(filter_with$series_type) & any(filtered_data$series_type == "Trend")) {
    filter_with$series_type <- "Trend"
  } else if (is.null(filter_with$series_type) & (!any(filtered_data$series_type == "Trend") & any(filtered_data$series_type == "Seasonally Adjusted"))) {
    filter_with$series_type <- "Seasonally Adjusted"
  } else if (is.null(filter_with$series_type)) {
    filter_with$series_type <- "Original"
  } else {filter_with$series_type = filter_with$series_type}


  filtered_data <- filtered_data %>%
    {if("series_type" %in% names(.)) dplyr::filter(., .data$series_type %in% filter_with['series_type']) else .}

  return(filtered_data)

}
