plot_parameters <- function(plot_data, states, indicator, sex = NULL, ages = NULL, series_types, compare_aus, facet) {

  plot_parameters <- list()

  if (length(states) == 1 & length(sex) > 1) {
    plot_parameters$col_var <- "gender"
    plot_parameters$n_cols <- length(sex)
    compare_aus <- FALSE
  } else if (length(states) == 1 & length(ages) > 1) {
    plot_parameters$col_var <- "age"
    plot_parameters$n_cols <- length(ages)
    compare_aus <- FALSE
  } else if (length(ages == 1) & length(sex) == 1) {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(states)
  } else {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(states)
  }



  if (compare_aus & !"Australia" %in% states) {
    states <- c(states, "Australia")
    plot_parameters$n_cols <- plot_parameters$n_cols + 1
  }

  to_match <- c("rate", "ratio", "proportion")

  if (grepl("payroll", indicator, ignore.case = TRUE)) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (length(states) >= 2 & !grepl(paste(to_match, collapse = "|"), indicator)) {
    plot_parameters$index <- TRUE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if ((length(states) == 1) & grepl(paste(to_match, collapse = "|"), indicator)) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_percent
  } else if (grepl(paste(to_match, collapse = "|"), indicator)) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else {
    plot_parameters$index <- FALSE
    plot_parameters$scale <- ifelse(min(plot_data$value > 1e6), 1e-6, 1)
    plot_parameters$suffix <- ifelse(min(plot_data$value > 1e6), "m", "")
    plot_parameters$y_label <- scales::comma_format(scale = plot_parameters$scale, suffix = plot_parameters$suffix)
    plot_parameters$hover <- as_comma
  }

  table_no <- dplyr::case_when(
    indicator == "Monthly hours worked in all jobs" ~ "19",
    indicator == "Underutilised total" ~ "23",
    indicator == "Underemployed total" ~ "23",
    indicator == "Underutilisation rate" ~ "23",
    indicator == "Underemployment rate" ~ "23",
    grepl("jobseeker|jobkeeper", indicator, ignore.case = TRUE) ~ "",
    grepl("payroll", indicator, ignore.case = TRUE) ~ "4",
    TRUE ~ "12"
  )

  series_types <- unique(series_types)


  caption_table <- dplyr::case_when(
    grepl("jobkeeper", indicator, ignore.case = TRUE) ~ paste0("Source: Treasury, ",
                                                               lubridate::month(max(plot_data$date), abbr = FALSE, label =TRUE), " ",
                                                               max(plot_data$year)),
    grepl("jobseeker", indicator, ignore.case = TRUE) ~ paste0("Source: Department of Social Services, ",
                                                               lubridate::month(max(plot_data$date), abbr = FALSE, label = TRUE), " ",
                                                               max(plot_data$year)),
    grepl("payroll", indicator, ignore.case = TRUE) ~ paste0("Source: ABS Weekly Payroll Jobs and Wages in Australia, ",
                                                             reportabs::release(plot_data, "month"), " ",
                                                             reportabs::release(plot_data, "year")),
    TRUE ~ paste0("Source: ABS Labour Force, Australia, ",
                  reportabs::release(plot_data, "month"), " ",
                  reportabs::release(plot_data, "year"),
                  " (Table ", table_no, ", ", series_types, ")")
  )


  plot_parameters$num_months <- as.numeric(max(plot_data$month))
  plot_parameters$month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$date))
  plot_parameters$caption <- caption_table

  if(plot_parameters$index) {
    plot_parameters$title <- toupper(paste0(indicator, ": ", paste0(strayr::clean_state(states), collapse = " & " )))
    plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
    plot_parameters$y_var <- "index"
  } else {
    plot_parameters$title <- toupper(paste0(indicator, ": ", paste0(strayr::clean_state(states), collapse = " & " )))
    plot_parameters$subtitle <- NULL
    plot_parameters$y_var <- "value"
  }

  if (!is.null(facet)) {
    plot_parameters$facet <- facet
  }


  return(plot_parameters)
}

create_plot <- function(plot_data, plot_parameters, void, plotly) {

  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes_(x = ~ date,
                                     y = as.name(plot_parameters$y_var),
                                     colour = as.name(plot_parameters$col_var))) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = min(plot_parameters$num_months, 6)), date_labels = "%b-%y") +
    ggplot2::scale_y_continuous(labels = plot_parameters$y_label) +
    aititheme::aiti_colour_manual(n = plot_parameters$n_cols)


  if (!void) {
    p <- p + ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_parameters$title,
      subtitle = plot_parameters$subtitle,
      caption = plot_parameters$caption
    ) + ggplot2::guides(colour = ggplot2::guide_legend()) +
      aititheme::theme_aiti(legend = 'bottom')
  } else {p <- p + ggplot2::theme_void() + ggplot2::theme(legend.position = "none")}

  if (!is.null(plot_parameters$facet)) {

    p <- p + ggplot2::facet_wrap(plot_parameters$facet)

  }

  if (plotly) {

    hover_format <- plot_parameters$hover


    p <- p +
      ggplot2::aes(group = 1,
                   text = paste0(.data$state,
                                 "<br>Gender: ", .data$gender,
                                 "<br>Age: ", .data$age,
                                 "<br>Date: ", format(date, "%b-%Y"),
                                 "<br>", .data$indicator, ": ", hover_format(.data$value))) +
      ggplot2::geom_point(shape = 1, size = 1) +
      aititheme::theme_aiti(legend = "bottom", base_family = "Roboto")

    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(autosize = TRUE,
                     legend = list(title = "",
                                   orientation = "h",
                                   y = -0.15),
                     annotations = list(
                       x = 1,
                       y = -0.2,
                       showarrow = FALSE,
                       xref = "paper",
                       yref = "paper",
                       xanchor = "right",
                       yanchor = "auto",
                       text = "Source: AITI Economic Indicators"
                     )

      )


  }

  return(p)

}
