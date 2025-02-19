plot_parameters <- function(plot_data, over, col_var, n_cols, markdown, compare_aus, facet) {

  plot_data <- dplyr::mutate(plot_data, value = ifelse(unit == "000", value * 1000, value))

  plot_parameters <- list()

  plot_parameters$col_var <- col_var
  plot_parameters$n_cols <- n_cols


  if (any(Map(length, over) > 1)) {
    plot_parameters$legend <- "bottom"
  }


  to_match <- c("rate", "ratio", "proportion")

  if (any(grepl("payroll", over$indicator, ignore.case = TRUE))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (length(over$state) >= 2 & !any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- TRUE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if ((length(over$state) == 1) & any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_percent
  } else if (any(grepl(paste(to_match, collapse = "|"), over$indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else {
    plot_parameters$index <- FALSE
    plot_parameters$scale <- ifelse(min(plot_data$value > 1e6), 1e-3, 1)
    plot_parameters$suffix <- dplyr::case_when(
      min(plot_data$value) > 1e6 ~ "m",
      min(plot_data$value) > 1e3 ~ "k",
      TRUE ~ "")
    plot_parameters$y_label <- scales::comma_format(scale = plot_parameters$scale, suffix = plot_parameters$suffix)
    plot_parameters$hover <- as_comma
  }

  table_no <- dplyr::case_when(
    v$indicator == "Monthly hours worked in all jobs" ~ "19",
    v$indicator == "Underutilised total" ~ "23",
    v$indicator == "Underemployed total" ~ "23",
    v$indicator == "Underutilisation rate" ~ "23",
    v$indicator == "Underemployment rate" ~ "23",
    grepl("jobseeker|jobkeeper", v$indicator, ignore.case = TRUE) ~ "",
    grepl("payroll", v$indicator, ignore.case = TRUE) ~ "4",
    TRUE ~ "12"
  )

  series_types <- unique(over$series_type)


  caption_table <- dplyr::case_when(
    grepl("jobkeeper", over$indicator, ignore.case = TRUE) ~ paste0("Source: Treasury, ",
                                                               lubridate::month(max(plot_data$date), abbr = FALSE, label =TRUE), " ",
                                                               lubridate::year(max(plot_data$date))),
    grepl("jobseeker", over$indicator, ignore.case = TRUE) ~ paste0("Source: Department of Social Services, ",
                                                               lubridate::month(max(plot_data$date), abbr = FALSE, label = TRUE), " ",
                                                               lubridate::year(max(plot_data$date))),
    grepl("payroll", over$indicator, ignore.case = TRUE) ~ paste0("Source: ABS Weekly Payroll Jobs and Wages in Australia, ",
                                                             reportabs::release(plot_data, "month"), " ",
                                                             reportabs::release(plot_data, "year")),
    TRUE ~ paste0("Source: ABS Labour Force, Australia, ",
                  reportabs::release(plot_data, "month"), " ",
                  reportabs::release(plot_data, "year"),
                  " (Table ", table_no, ", ", series_types, ")")
  )


  plot_parameters$num_months <- as.numeric(lubridate::month(max(plot_data$date)))
  plot_parameters$month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$date))
  plot_parameters$caption <- caption_table
  plot_parameters$date_range <- c(min(plot_data$date), max(plot_data$date))
  plot_parameters$markdown <- markdown

  if (plot_parameters$markdown & plot_parameters$col_var == "state") {
  title_cols <- aititheme::aiti_pal()(plot_parameters$n_col)
  names(title_cols) <- states

  plot_title_md <- paste0("<span style = color:'", title_cols, "'>", names(title_cols), "</span>", collapse = " and ")

  } else {
    plot_title_md <- paste0(over$state, collapse = " & ")
  }
  if(plot_parameters$index) {
    plot_parameters$title <- stringr::str_to_title(over$indicator) #paste0(stringr::str_to_title(indicator), ": ", plot_title_md)
    plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
    plot_parameters$y_var <- "index"
  } else {
    plot_parameters$title <- stringr::str_to_title(over$indicator) #paste0(stringr::str_to_title(indicator), ": ", plot_title_md)
    plot_parameters$subtitle <- NULL
    plot_parameters$y_var <- "value"
  }

  if (!is.null(facet)) {
    plot_parameters$facet <- facet
  }


  return(plot_parameters)
}



create_plot <- function(plot_data, plot_parameters, void, plotly, ...) {

  date_min <- as.Date(plot_parameters$date_range[1])
  date_max <- as.Date(plot_parameters$date_range[2])

  pre <- scales::breaks_pretty(n = 5)(c(date_min, date_max))
  date_adj <- as.numeric(pre[length(pre)] - date_max)
  adj <- pre - date_adj
  names(adj) <- NULL
  date_breaks <- adj[adj >= date_min & adj <= date_max]


  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = date,
                                     y = .data[[plot_parameters$y_var]],
                                     colour = .data[[plot_parameters$col_var]])) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_date(date_labels = "%e %b\n%Y",
                          breaks = date_breaks) +
    ggplot2::scale_y_continuous(labels = plot_parameters$y_label)

  if (!void) {
    p <- p + ggplot2::labs(
      x = NULL,
      y = NULL,
      #title = plot_parameters$title,
      subtitle = plot_parameters$subtitle,
      caption = plot_parameters$caption
    ) + ggplot2::guides(colour = ggplot2::guide_legend())

    if (requireNamespace("aititheme", quietly = TRUE)) {
      p <- p + aititheme::theme_aiti(...) + aititheme::scale_colour_aiti()
    }
  } else {
    p <- p + ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
    }

  if (!is.null(plot_parameters$facet)) {

    p <- p + ggplot2::facet_wrap(plot_parameters$facet)

  }

  if (plotly) {

    hover_format <- plot_parameters$hover


    p <- p +
      ggplot2::aes(group = 1,
                   text = paste0(.data$state,
                                 "<br>Sex: ", .data$sex,
                                 "<br>Age: ", .data$age,
                                 "<br>Date: ", format(date, "%b-%Y"),
                                 "<br>", .data$indicator, ": ", hover_format(.data$value))) +
      ggplot2::geom_point(shape = 1, size = 1)

    p <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(autosize = TRUE,
                     legend = list(title = "X",
                                   orientation = "h",
                                   y = -0.5),
                     margin = list(autoexpand = TRUE),
                     annotations = list(
                       x = 1,
                       y = -0.5,
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
