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
#'
#'

abs_plot <- function(indicators,
                     states,
                     years = 5,
                     ages = "Total (age)",
                     genders = "Persons",
                     series_types = "Seasonally Adjusted",
                     compare_aus = TRUE,
                     plotly = FALSE) {

  #Error checking - only one variable is allowed to be of length > 1

  if ((length(ages) > 1 & length(states) > 1 ) |
    (length(genders) > 1 & length(states) > 1) |
    (length(ages) > 1 & length(genders) > 1)) {
    stop("You can't combine multiple states with multiple other variables")
  }

  #Determine what is being plot

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
  }

  #Should Australia be added?
  if (compare_aus & !"Australia" %in% states) {
    states <- c("Australia", states)
    n_cols <- n_cols + 1
  }

  #Should the plot be indexed?
  #Index if: Comparing 2 or more states & the indicator is not a rate

  if (length(states) >= 2 & stringr::str_detect(indicators, "rate", negate = TRUE)) {
    plot_index <- TRUE
    y_label <- scales::percent_format(scale = 1)
  } else {
    plot_index <- FALSE
    y_label <- scales::comma_format()
  }

  plot_data <- labour_force %>%
    dplyr::filter(indicator == indicators,
                  gender %in% genders,
                  series_type == series_types,
                  age %in% ages,
                  year >= max(.$year) - years) %>%
    dplyr::group_by(state, gender, age) %>%
    dplyr::mutate(index = 100 * value / value[1]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(state %in% states)

  plot_month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_year <- lubridate::year(min(plot_data$date))
  plot_caption <- stringr::str_c("Source: 6202.0 - Labour Force, Australia, ",
                                 release(labour_force, "month"), " ",
                                 release(labour_force, "year"),
                                 " (Table X, ",
                                 series_types, ")")

  if(plot_index) {
    plot_title <- stringr::str_to_upper(stringr::str_c(indicators, ": ",
                                                       stringr::str_c(strayr::strayr(states), collapse = " & " )))

    plot_subtitle <- paste("Index (Base:", plot_month, plot_year, "= 100%)")
    y_var <- "index"
  } else {
    plot_title <- stringr::str_to_upper(stringr::str_c(indicators, ": ",
                                                       stringr::str_c(strayr::strayr(states), collapse = " & " )))
    plot_subtitle <- NULL
    y_var <- "value"
  }

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes_(x = ~ date,
                                     y = as.name(y_var),
                                     colour = as.name(col_var),
                                     linetype = ~ state)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_title,
      subtitle = plot_subtitle,
      caption = plot_caption
    ) +
    ggplot2::scale_x_date(date_labels = "%b-%Y") +
    ggplot2::scale_y_continuous(labels = y_label) +
    ggplot2::guides(colour = ggplot2::guide_legend()) +
    aititheme::aiti_colour_manual(n = n_cols) +
    aititheme::theme_aiti(legend = 'bottom')

  if(plotly) {

    hover_format <- ifelse(plot_data$unit[1] == "000", as_comma, as_percent)


    p <- p +
      ggplot2::aes(group = 1,
                   text = stringr::str_c(state,
                                 "<br>Gender: ", gender,
                                 "<br>Age: ", age,
                                 "<br>Date: ", format(date, "%b-%Y"),
                                 "<br>", indicator, ": ", hover_format(value)))

    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(autosize = TRUE,
                     legend = list(orientation = "h",
                                   y = -0.15),
                     annotations = list(
                         x = 1,
                         y = 0,
                         text = "Source: WorkSight",
                         showarrow = F,
                         xref = "paper",
                         yref = "paper",
                         xanchor = "right",
                         yanchor = "auto",
                         xshift = 0,
                         yshift = -0.15))

  }

  return(p)

}




