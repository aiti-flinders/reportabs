filter_list <- function(data, over) {

  # filter_list is an attempt at a less restrictive filter_with
  # if v is asking to filter on a variable that isnt present in data, just drop it.
  # but maybe we still want some defaults if not specified.

  if(!is.list(over)) {
    stop("`over` needs to be a named list")
  }

  over_safe <- make_safe(data, over)

  l <- purrr::imap(over_safe, ~data[data[[.y]] %in% .x, ])

  suppressMessages(Reduce(dplyr::inner_join, l))
}


make_safe <- function(data, over) {

  over_safe <- over[names(over) %in% names(data)]

  if (any(!over %in% over_safe)) {
    #message("i dropped a variable")
  }

  if (any(names(data) == "sex") && (!"sex" %in% names(over_safe)))  {

    #message("implied sex = 'Persons'")

    over_safe$sex = "Persons"

  }

  if (any(names(data) == "series_type") && (!"series_type" %in% names(over_safe))) {

    #message("implied series_type = 'Trend'")

    over_safe$series_type = "Trend"
  }

  if (any(names(data) == "state") && (!"state" %in% names(over_safe))) {

    #message("implied state = 'Australia'")

    over_safe$state = "Australia"
  }

  if (any(names(data) == "age") && (!"age" %in% names(over_safe))) {

    #message("implied age = 'Total (age)'")
    over_safe$age = "Total (age)"
  }

  if (any(names(data) == "industry") && (!"industry" %in% names(over_safe))) {
    over_safe$industry = unique(strayr::anzsic2006$anzsic_division)
  }


  # if (any(v$indicator %in% c("Monthly hours worked in all jobs (employed full-time)",
  #                          "Monthly hours worked in all jobs (employed part-time)",
  #                          "Employed part-time",
  #                          "Unemployed looked for full-time work",
  #                          "Unemployed looked for only part-time work",
  #                          "Unemployment rate looked for full-time work",
  #                          "Unemployment rate looked for only part-time work",
  #                          "Jobkeeper applications"))) {
  #   v_safe$series_type <- "Original"
  # }


  return(over_safe)

}
