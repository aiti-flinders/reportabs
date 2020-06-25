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
           series_types = "Seasonally Adjusted",
           plot_index = TRUE) {
    if (length(states) == 1 & length(genders) > 1) {
      col_var <- "gender"
      n_cols <- length(genders)
      compare_aus <- FALSE
    } else if (length(states) == 1 & length(ages) > 1) {
      col_var <- "age"
      n_cols <- length(ages)
      compare_aus <- FALSE
    } else if (length(ages == 1) & length(genders) == 1) {
      col_var <- "state"
      n_cols <- length(states)
    } else if (
      (length(ages) > 1 & length(states) > 1) |
      (length(genders) > 1 & length(states) > 1) |
      (length(ages) > 1 & length(genders) > 1)) {
      stop("You can't combine multiple states with multiple other variables")
    }

    if ("Australia" %in% states & length(states) == 1) {
      #Only Australia has been requested - no need to compare, or index
      compare_aus <- FALSE
      plot_index <- FALSE
    }

    if (compare_aus == TRUE & !"Australia" %in% states) {
      states <- c(states, "Australia")
      n_cols <- n_cols + 1
    } else if (compare_aus == FALSE) {
      plot_index <- TRUE
    }

    if (length(states) < 2) {plot_index <- FALSE} else plot_index <- TRUE

    plot_data <- labour_force %>%
      dplyr::filter(
        indicator == "Monthly hours worked in all jobs",
        gender == genders,
        age == ages,
        series_type == series_types,
        year >= max(.$year) - years
      ) %>%
      dplyr::group_by(state, gender, age) %>%
      dplyr::mutate(index = 100 * value / value[1]) %>%
      dplyr::ungroup()


    plot_month <-
      lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
    plot_year <- lubridate::year(min(plot_data$date))

    if (plot_index) {
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
                        colour = as.name(col_var),
                        linetype = as.name(col_var)
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
      aititheme::aiti_colour_manual(n = n_cols) +
      ggplot2::guides(linetype = ggplot2::guide_legend(),
                      colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')

    return(plot)

  }
