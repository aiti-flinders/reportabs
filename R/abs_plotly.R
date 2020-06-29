#' Draw interactive abs plots
#'
#' @param indicator string. One of:
#'
#' \itemize{
#' \item{"Employment"}
#' \item{"Unemployment"}
#' \item{"Participation rate"}
#' \item{"Unemployment rate"}
#' }
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

abs_plotly <- function(indicator,
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

    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Total Employment: ", reportabs::as_comma(value)))

  } else if (indicator == "Unemployment rate") {
    ret <- abs_unemployment_rate(states = states,
                                 years = years,
                                 compare_aus = compare_aus,
                                 ages = ages,
                                 genders = genders,
                                 series_type = series_type)

    ret <- ret +
      aes(group = states, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Unemployment Rate: ", reportabs::as_percent(value)))

  } else if (indicator == "Unemployment") {
    ret <- abs_unemployment(states = states,
                            years = years,
                            compare_aus = compare_aus,
                            ages = ages,
                            genders = genders,
                            series_type = series_type)

    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Unemployment Total: ", reportabs::as_comma(value)))

  } else if (indicator == "Participation rate") {
    ret <- abs_participation_rate(states = states,
                                  years = years,
                                  compare_aus = compare_aus,
                                  ages = ages,
                                  genders = genders,
                                  series_type= series_type)

    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Participation Rate: ", reportabs::as_percent(value)))

  } else if (indicator == "Underemployment rate") {
    ret <- abs_underemployment_rate(states = states,
                                    years = years,
                                    compare_aus = compare_aus,
                                    ages = ages,
                                    genders = genders,
                                    series_type = series_type)

    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Underemployment Rate: ", reportabs::as_percent(value)))

  } else if (indicator == "Underemployment") {
    ret <- abs_underemployment(states = states,
                               years = years,
                               compare_aus = compare_aus,
                               ages = ages,
                               genders = genders,
                               series_type = series_type)
    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Underemployed Total: ", reportabs::as_comma(value)))

  } else if (indicator == "Underutilisation rate") {
    ret <- abs_underutilisation_rate(states = states,
                                     years = years,
                                     compare_aus = compare_aus,
                                     ages = ages,
                                     genders = genders,
                                     series_type = series_type)
    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Underutilisation Rate: ", reportabs::as_percent(value)))

  } else if (indicator == "Underutilisation") {
    ret <- abs_underutilisation(states = states,
                                years = years,
                                compare_aus = compare_aus,
                                ages = ages,
                                genders = genders,
                                series_type = series_type)
    ret <- ret +
      aes(group = 1, #required with text aesthetic
          text = stringr::str_c(states,
                                "<br>Gender: ", genders,
                                "<br>Age: ", ages,
                                "<br>Date: ", format(date, "%Y-%b"),
                                "<br>Underutilised Total: ", reportabs::as_comma(value))) +
      aititheme::theme_aiti(legend = 'bottom')

  }

  retly <- plotly::ggplotly(ret, tooltip = "text") %>%
    plotly::layout(autosize = TRUE,
                   legend = list(orientation = "h",
                                 y = -0.15),
                   xaxis = list(automargin = TRUE))

  return(retly)


}
