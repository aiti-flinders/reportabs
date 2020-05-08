#' Title
#'
#' @return filtered dataframe
#' @export
#'
#' @examples
filter_with <- function(data, filter_with = NULL) {
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
    dplyr::filter(indicator %in% filter_with$indicator) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., age == filter_with['age']) else .}

  #Can only check if the indicator has a Trend series after it has been filtered.
  if(is.null(filter_with$series_type) & any(filtered_data$series_type == "Trend")) {
    filter_with$series_type <- "Trend"
  } else if (is.null(filter_with$series_type) & !any(filtered_data$series_type == "Trend")) {
    filter_with$series_type <- "Original"
  }

  filtered_data <- filtered_data %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .}

  return(filtered_data)

}