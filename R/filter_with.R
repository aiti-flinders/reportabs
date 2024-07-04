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
