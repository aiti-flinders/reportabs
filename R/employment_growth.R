employment_growth <- function(data = NULL, filter_with = list(), year_since = 2010) {

  if(is.null(data)) {data <- reportabs::underutilisation}

  cols <- c("Employed full-time" = "#041457", "Employed part-time" = "#006eff", "Employed total" = "#a1a1a1")

  if(!is.list(filter_with)) {
    stop("Function requires a list!")
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

  if(is.null(filter_with$industry)) {
    filter_with$industry = "Total all industries"
  }


  if(is.null(filter_with$series_type) & any(data$series_type == "Trend")) {
    filter_with$series_type = "Trend"
  } else {
    filter_with$series_type = "Original"
  }


  p_emp_growth <- data %>%
    dplyr::filter(indicator %in% c("Employed total", "Employed part-time", "Employed full-time")) %>%
    {if("gender" %in% names(.)) dplyr::filter(., gender == filter_with['gender']) else .} %>%
    {if("state" %in% names(.)) dplyr::filter(., state == filter_with['state']) else .} %>%
    {if("series_type" %in% names(.)) dplyr::filter(., series_type == filter_with['series_type']) else .} %>%
    {if("age" %in% names(.)) dplyr::filter(., age == filter_with['age']) else .} %>%
    {if("industry" %in% names(.)) dplyr::filter(., industry == filter_with['industry']) else .} %>%
    dplyr::group_by(year, indicator) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(value = (value - dplyr::lag(value))/value) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= year_since) %>%
    ggplot2::ggplot(ggplot2::aes(x = as.factor(year), y = value, fill = indicator)) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_fill_manual(values = cols, labels = c("Full Time", "Part Time", "Total"), name = "Employment Type") +
    ggplot2::labs(x = NULL,
      y = "Average Annual Growth (%)",
      caption = stringr::str_c("Source: ABS 6202.0, ", format(max(data$date), "%B %Y")))

  return(p_emp_growth)

}
