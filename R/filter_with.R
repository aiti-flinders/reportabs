filter_list <- function(data, over) {

  if (!is.list(over)) {
    cli::cli_abort("The variable 'over' must be a named list")
  }

  if (!"indicator" %in% names(over)) {
    cli::cli_abort("The variable 'over' must include an indicator")
  }

  over_safe <- make_safe(data, over)

  l <- purrr::map(over_safe, length) |>
    purrr::list_c()

  build_exprs <- over_safe |>
    tibble::enframe() |>
    dplyr::mutate(l = l,
                  expr = dplyr::case_when(
                    l == 1 ~ paste0(name, " == ", "'", value, "'"),
                    l > 1 ~ paste0(name, " %in% ", value)
                  )
    )

  dplyr::filter(data, !!!rlang::parse_exprs(build_exprs$expr))

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
    over_safe$industry = unique(data$industry)
  }



  return(over_safe)

}
