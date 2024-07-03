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
    dplyr::filter(.data$indicator %in% filter_with$indicator,
                  dplyr::if_any(dplyr::matches("gender"), ~ .x %in% filter_with$gender),
                  dplyr::if_any(dplyr::matches("state"), ~ .x %in% filter_with$state),
                  dplyr::if_any(dplyr::matches("age"), ~.x %in% filter_with$age))


  #Can only check if the indicator has a Trend series after it has been filtered.

  if(is.null(filter_with$series_type) & any(filtered_data$series_type == "Trend")) {
    filter_with$series_type <- "Trend"
  } else if (is.null(filter_with$series_type) & (!any(filtered_data$series_type == "Trend") & any(filtered_data$series_type == "Seasonally Adjusted"))) {
    filter_with$series_type <- "Seasonally Adjusted"
  } else if (is.null(filter_with$series_type)) {
    filter_with$series_type <- "Original"
  } else {filter_with$series_type = filter_with$series_type}

  if ("series_type" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(.data$series_type %in% filter_with$series_type)
  }

  return(filtered_data)

}

filter_list <- function(data, v) {

  # filter_list is an attempt at a less restrictive filter_with
  # if v is asking to filter on a variable that isnt present in data, just drop it.
  # but maybe we still want some defaults if not specified.

  if(!is.list(v)) {
    stop("`v` needs to be a named list")
  }

  v_safe <- make_safe(data, v)

  l <- purrr::imap(v_safe, ~data[data[[.y]] %in% .x, ])

  suppressMessages(Reduce(dplyr::inner_join, l))
}


make_safe <- function(data, v) {

  v_safe <- v[names(v) %in% names(data)]

  if (any(!v %in% v_safe)) {
    #message("i dropped a variable")
  }

  if (any(names(data) == "sex") && (!"sex" %in% names(v_safe)))  {

    #message("implied sex = 'Persons'")

    v_safe$sex = "Persons"

  }

  if (any(names(data) == "series_type") && (!"series_type" %in% names(v_safe))) {

    #message("implied series_type = 'Trend'")

    v_safe$series_type = "Trend"
  }

  if (any(names(data) == "state") && (!"state" %in% names(v_safe))) {

    #message("implied state = 'Australia'")

    v_safe$state = "Australia"
  }

  if (any(names(data) == "age") && (!"age" %in% names(v_safe))) {

    #message("implied age = 'Total (age)'")
    v_safe$age = "Total (age)"
  }


  if (any(v$indicator %in% c("Monthly hours worked in all jobs (employed full-time)",
                           "Monthly hours worked in all jobs (employed part-time)",
                           "Employed part-time",
                           "Unemployed looked for full-time work",
                           "Unemployed looked for only part-time work",
                           "Unemployment rate looked for full-time work",
                           "Unemployment rate looked for only part-time work",
                           "Jobkeeper applications"))) {
    v_safe$series_type <- "Original"
  }


  return(v_safe)

}
