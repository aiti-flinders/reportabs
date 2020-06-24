#' Plot levels of employment
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one. Plotting
#' more than one state will index the data to January of the first year included in the data.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian employment to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_employment
#'
#' @examples The last 5 years employment in South Australia: abs_employment("South Australia")
abs_employment <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Seasonally Adjusted") {
    if (states == "Australia") {
      compare_aus = FALSE
    }


    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Employed total",
        gender %in% genders,
        series_type == series_types,
        age %in% ages,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("EMPLOYMENT: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      subtitle <-
        paste("Index (Base:", plot_month, plot_year, "= 100)")
      y_var <- "index"
    } else {
      plot_title <-
        stringr::str_c("EMPLOYMENT: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      subtitle <- NULL
      y_var <- "value"
    }

    plot_caption <-
      stringr::str_c(
        "Source: 6202.0 - Labour Force, Australia, ",
        release(labour_force, 'month'),
        " ",
        release(labour_force, 'year'),
        " (Table 12, ",
        series_types,
        ")"
      )

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = plot_title,
        subtitle = subtitle,
        caption =  plot_caption
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')

    return(plot)

  }

#' Plot unemployment levels
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one. Plotting
#' more than one state will index the data to January of the first year included in the data.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian unemployment to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_unemployment
#'
#' @examples The last 5 years unemployment in South Australia: abs_unemployment("South Australia")
abs_unemployment <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Seasonally Adjusted") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Unemployed total",
        gender == genders,
        age == ages,
        series_type == series_types,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("UNEMPLOYMENT: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      subtitle <-
        paste("Index (Base:", plot_month, plot_year, "= 100)")
      y_var <- "index"
    } else {
      plot_title <-
        stringr::str_c("UNEMPLOYMENT: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      subtitle <- NULL
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        title = plot_title,
        subtitle = subtitle,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 12, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')


    return(plot)

  }

#' Plot unemployment rates
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian unemployment rate to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_unemployment_rate
#'
#' @examples The last 5 years unemployment rate in South Australia: abs_unemployment_rate("South Australia")
abs_unemployment_rate <- function(states,
                                  years = 5,
                                  compare_aus = TRUE,
                                  ages = "Total (age)",
                                  genders = "Persons",
                                  series_types = "Seasonally Adjusted") {
  if (states == "Australia") {
    compare_aus = FALSE
  }

  plot_data <- labour_force %>%
    dplyr::filter(
      indicator == "Unemployment rate",
      gender == genders,
      age == ages,
      series_type == series_types,
      year >= max(.$year) - years
    )

  plot_month <-
    lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))

  if (compare_aus) {
    plot_title <-
      stringr::str_c("UNEMPLOYMENT RATE: ",
                     stringr::str_to_upper(strayr::strayr(states)),
                     " & AUSTRALIA")
    plot_data <- plot_data %>%
      dplyr::filter(state %in% c(states, "Australia"))
    y_var <- "value"
  } else {
    plot_title <-
      stringr::str_c("UNEMPLOYMENT RATE: ", stringr::str_to_upper(states))
    plot_data <- plot_data %>%
      dplyr::filter(state %in% states)
    y_var <- "value"
  }

  plot <-
    ggplot2::ggplot(plot_data,
                    ggplot2::aes_(
                      x = ~ date,
                      y = as.name(y_var),
                      colour = ~ state,
                      linetype = ~ state
                    )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_title,
      caption = stringr::str_c(
        "Source: 6202.0 - Labour Force, Australia, ",
        release(labour_force, 'month'),
        " ",
        release(labour_force, 'year'),
        " (Table 12, ",
        series_types,
        ")"
      )
    ) +
    ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                          labels = scales::date_format("%b-%y")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_colour_manual(
      breaks = c(states, "Australia"),
      values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
    ) +
    ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                   values = c("solid", "dashed")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(),
                    colour = ggplot2::guide_legend()) +
    aititheme::theme_aiti(legend = 'bottom')

  return(plot)
}

#'Plot monthly hours worked in all jobs
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one. Plotting
#' more than one state will index the data to January of the first year included in the data.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian hours worked to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_hoursworked
#'
#' @examples The last 5 years hours worked in South Australia: abs_hoursworked("South Australia")
abs_hoursworked <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Seasonally Adjusted") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Monthly hours worked in all jobs",
        gender == genders,
        age == ages,
        series_type == series_types,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()


    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("HOURS WORKED: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      subtitle <-
        paste("Index (Base:", plot_month, plot_year, "= 100)")
      y_var <- "index"
    } else {
      plot_title <-
        stringr::str_c("HOURS WORKED: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      subtitle <- NULL
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        title = plot_title,
        subtitle = subtitle,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 19, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')

    return(plot)

  }

#' Plot Underutilisation levels
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one. Plotting
#' more than one state will index the data to January of the first year included in the data.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian underutilisation to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_underutilisation
#'
#' @examples The last 5 years hours worked in South Australia: abs_hoursworked("South Australia")
abs_underutilisation <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Seasonally Adjusted") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Underutilised total",
        gender == genders,
        age == ages,
        series_type == series_types,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("UNDERUTILISATION: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      subtitle <-
        paste("Index (Base:", plot_month, plot_year, "= 100)")
      y_var <- "index"
    } else {
      plot_title <-
        stringr::str_c("UNDERUTILISATION: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      subtitle <- NULL
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        title = plot_title,
        subtitle = subtitle,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 23, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')

    return(plot)
  }

#' Plot Underutilisation rates
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian underutilisation rates to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_underutilisation_rate
#'
#' @examples The last 5 years hours worked in South Australia: abs_hoursworked("South Australia")
abs_underutilisation_rate <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Trend") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Underutilisation rate",
        gender == "Persons",
        age == "Total (age)",
        series_type == series_types,
        year >= max(.$year) - years
      )

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c(
          "UNDERUTILISATION RATE: ",
          stringr::str_to_upper(strayr::strayr(states)),
          " & AUSTRALIA"
        )
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      y_var <- "value"
    } else {
      plot_title <-
        stringr::str_c("UNDERUTILISATION RATE: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = plot_title,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 23, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')
    return(plot)
  }



#'Underemployment rate
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian underemployment rate to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_underemployment_rate
#'
#' @examples The last 5 years underemployment rate in South Australia: abs_underemployment_rate("South Australia")
#'
abs_underemployment_rate <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Trend") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Underemployment rate (proportion of labour force)",
        gender == "Persons",
        age == "Total (age)",
        series_type == series_types,
        year >= max(.$year) - years
      )

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("UNDEREMPLOYMENT RATE: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      y_var <- "value"
    } else {
      plot_title <-
        stringr::str_c("UNDEREMPLOYMENT RATE: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = plot_title,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 23, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale =
                                                                    1)) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')

    return(plot)
  }

#'Plot Underemployment Levels
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one. Plotting
#' more than one state will index the data to January of the first year included in the data.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian underemployment to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_underemployment
#'
#' @examples The last 5 years underemployment in South Australia: abs_underemployment("South Australia")
abs_underemployment <-
  function(states,
           years = 5,
           compare_aus = TRUE,
           ages = "Total (age)",
           genders = "Persons",
           series_types = "Seasonally Adjusted") {
    if (states == "Australia") {
      compare_aus = FALSE
    }

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Underemployed total",
        gender == genders,
        age == ages,
        series_type == series_types,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()

    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (compare_aus) {
      plot_title <-
        stringr::str_c("UNDEREMPLOYMENT: ",
                       stringr::str_to_upper(strayr::strayr(states)),
                       " & AUSTRALIA")
      plot_data <- plot_data %>%
        dplyr::filter(state %in% c(states, "Australia"))
      subtitle <-
        paste("Index (Base:", plot_month, plot_year, "= 100)")
      y_var <- "index"
    } else {
      plot_title <-
        stringr::str_c("UNDEREMPLOYMENT: ", stringr::str_to_upper(states))
      plot_data <- plot_data %>%
        dplyr::filter(state %in% states)
      subtitle <- NULL
      y_var <- "value"
    }

    plot <-
      ggplot2::ggplot(plot_data,
                      ggplot2::aes_(
                        x = ~ date,
                        y = as.name(y_var),
                        colour = ~ state,
                        linetype = ~ state
                      )) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = NULL,
        title = plot_title,
        subtitle = subtitle,
        caption = stringr::str_c(
          "Source: 6202.0 - Labour Force, Australia, ",
          release(labour_force, 'month'),
          " ",
          release(labour_force, 'year'),
          " (Table 23, ",
          series_types,
          ")"
        )
      ) +
      ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                            labels = scales::date_format("%b-%y")) +
      ggplot2::scale_y_continuous(labels = scales::comma_format()) +
      ggplot2::scale_colour_manual(
        breaks = c(states, "Australia"),
        values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
      ) +
      ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                     values = c("solid", "dashed")) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')


    return(plot)
  }

#' Convenience function to draw a bar chart of employment growth, by different types
#'
#' @param states Which state to plot
#' @param series_types Which series type to plot
#' @param since Growth of employment since a given year
#'
#' @return ggplot2 object
#' @export employment_growth
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples employment_growth("South Australia", "Seasonally Adjusted", since = 2005)
#'
employment_growth <- function(states, series_types,  since = 2010) {
  plot_data <- labour_force %>%
    dplyr::filter(
      indicator %in% c("Employed total", "Employed full-time", "Employed part-time"),
      state == states,
      age == "Total (age)",
      series_type == series_types,
      gender == "Persons"
    ) %>%
    dplyr::group_by(year, indicator) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(value = (value - dplyr::lag(value)) / value) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year >= years)

  p_emp_growth <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = as.factor(year),
      y = value,
      fill = indicator
    )) +
    ggplot2::geom_bar(stat = 'identity', position = 'dodge') +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      x = NULL,
      subtitle = "Average Annual Growth (%)",
      title = stringr::str_c("EMPLOYMENT GROWTH: ", stringr::str_to_upper(states)),
      caption = stringr::str_c(
        "Source: 6202.0 - Labour Force, Australia, ",
        release(labour_force, 'month'),
        " ",
        release(labour_force, 'year'),
        " (Table 12, ",
        series_types,
        ")"
      )
    ) +
    aititheme::theme_aiti(legend = 'bottom')

  return(p_emp_growth)

}

#' Draw employment by industry bar chart, coloured by underemployment levels. This function takes no inputs
#'
#' @return ggplot2 object
#' @export underemployment_industry
#'
#' @examples underemployment_industry()
underemployment_industry <- function() {
  data <- reportabs::employment_industry %>%
    dplyr::filter(
      indicator %in% c(
        "Underemployment ratio (proportion of employed)",
        "Employed total"
      ),
      state == "Australia",
      gender == "Persons",
      age == "Total (age)",
      industry != "Total (industry)"
    ) %>%
    dplyr::group_by(year, industry, indicator) %>%
    dplyr::summarise(value = mean(value) / 1e6) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::filter(year == max(.$year))

  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = reorder(industry, `Employed total`),
        y = `Employed total`,
        fill = `Underemployment ratio (proportion of employed)`
      )
    ) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::coord_flip() +
    ggplot2::labs(subtitle = "Total Employment (Millions)",
                  x = NULL) +
    aititheme::theme_aiti(legend = 'none')
  return(plot)

}

#' Plot participation rates
#'
#' @param states Which states/territories to plot. Accepts a character vector if plotting more than one.
#' @param years Over how many years the plot should span. Defaults to 5.
#' @param compare_aus Whether to add Australian participation rate to the plot. Default TRUE except if states = "Australia"
#' @param ages What age group should be plot. This option is only available if states = "Australia". Defaults to all ages
#' @param genders What genders should be plot. Defaults to Persons
#' @param series_types Which series type should be plot. Defaults to Seasonally Adjusted
#'
#' @return a ggplot2 object
#' @export abs_participation_rate
#'
#' @examples The last 5 years unemployment rate in South Australia: abs_participation_rate("South Australia")
abs_participation_rate <- function(states,
                                  years = 5,
                                  compare_aus = TRUE,
                                  ages = "Total (age)",
                                  genders = "Persons",
                                  series_types = "Seasonally Adjusted") {
  if (states == "Australia") {
    compare_aus = FALSE
  }

  plot_data <- labour_force %>%
    dplyr::filter(
      indicator == "Participation rate",
      gender == genders,
      age == ages,
      series_type == series_types,
      year >= max(.$year) - years
    )

  plot_month <-
    lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))

  if (compare_aus) {
    plot_title <-
      stringr::str_c("PARTICIPATION RATE: ",
                     stringr::str_to_upper(strayr::strayr(states)),
                     " & AUSTRALIA")
    plot_data <- plot_data %>%
      dplyr::filter(state %in% c(states, "Australia"))
    y_var <- "value"
  } else {
    plot_title <-
      stringr::str_c("PARTICIPATION RATE: ", stringr::str_to_upper(states))
    plot_data <- plot_data %>%
      dplyr::filter(state %in% states)
    y_var <- "value"
  }

  plot <-
    ggplot2::ggplot(plot_data,
                    ggplot2::aes_(
                      x = ~ date,
                      y = as.name(y_var),
                      colour = ~ state,
                      linetype = ~ state
                    )) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_title,
      caption = stringr::str_c(
        "Source: 6202.0 - Labour Force, Australia, ",
        release(labour_force, 'month'),
        " ",
        release(labour_force, 'year'),
        " (Table 12, ",
        series_types,
        ")"
      )
    ) +
    ggplot2::scale_x_date(date_breaks = date_breaks_format(years),
                          labels = scales::date_format("%b-%y")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_colour_manual(
      breaks = c(states, "Australia"),
      values = c(aititheme::aiti_darkblue, aititheme::aiti_lightblue)
    ) +
    ggplot2::scale_linetype_manual(breaks = c(states, "Australia"),
                                   values = c("solid", "dashed")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(),
                    colour = ggplot2::guide_legend()) +
    aititheme::theme_aiti(legend = 'bottom')

  return(plot)
}

