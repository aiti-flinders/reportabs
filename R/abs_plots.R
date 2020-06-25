#' #This function calls any of the predefined plots in plots.R for convenience
#'
#' @param indicator
#' @param states
#' @param years
#' @param compare_aus
#' @param ages
#' @param genders
#' @param series_type
#'
#' @return
#' @export
#'
#' @examples
abs_plot <- function(indicator,
                     states,
                     years = 5,
                     compare_aus = TRUE,
                     ages = "Total (age)",
                     genders = "Persons",
                     series_type = "Seasonally Adjusted") {

    if ("Australia" %in% states) {
      compare_aus = FALSE
    }

    #Don't want to punish people for capitalisation - so convert everything to sentence case:

    indicator <- stringr::str_to_sentence(indicator)


    if (indicator == "Employment") {
      ret <- abs_employment(states = states,
                            years = years,
                            compare_aus = compare_aus,
                            ages = ages,
                            genders = genders,
                            series_type = series_type)
    } else if (indicator == "Unemployment rate") {
      ret <- abs_unemployment_rate(states = states,
                                   years = years,
                                   compare_aus = compare_aus,
                                   ages = ages,
                                   genders = genders,
                                   series_type = series_type)
    } else if (indicator == "Unemployment") {
      ret <- abs_unemployment(states = states,
                              years = years,
                              compare_aus = compare_aus,
                              ages = ages,
                              genders = genders,
                              series_type = series_type)
    } else if (indicator == "Participation rate") {
      ret <- abs_participation_rate(states = states,
                                    years = years,
                                    compare_aus = compare_aus,
                                    ages = ages,
                                    genders = genders,
                                    series_type= series_type)
    } else if (indicator == "Underemployment rate") {
      ret <- abs_underemployment_rate(states = states,
                                      years = years,
                                      compare_aus = compare_aus,
                                      ages = ages,
                                      genders = genders,
                                      series_type = series_type)
    } else if (indicator == "Underemployment") {
      ret <- abs_underemployment(states = states,
                                 years = years,
                                 compare_aus = compare_aus,
                                 ages = ages,
                                 genders = genders,
                                 series_type = series_type)
    } else if (indicator == "Underutilisation rate") {
      ret <- abs_underutilisation_rate(states = states,
                                       years = years,
                                       compare_aus = compare_aus,
                                       ages = ages,
                                       genders = genders,
                                       series_type = series_type)
    } else if (indicator == "Underutilisation") {
      ret <- abs_underutilisation(states = states,
                                  years = years,
                                  compare_aus = compare_aus,
                                  ages = ages,
                                  genders = genders,
                                  series_type = series_type)
    }

    return(ret)
}
