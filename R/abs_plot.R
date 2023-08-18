#' Generate a time-series plot for ABS labour force indicators.
#'
#' \code{abs_plot()} can automatically generate appropriate plots for ABS
#' Time Series indicators for both static display in documents, or RMarkdown,
#' as well as interactive plots through plotly.
#'
#' @param indicator string. One or more of the indicators to be plot from the labour_force dataset. Allowed values are:
#' \itemize{
#' \item{Employed total}
#' \item{Employed full-time}
#' \item{Employed part-time}
#' \item{Unemployed total}
#' \item{Unemployed looked for full-time work}
#' \item{Unemployed looked for only part-time work}
#' \item{Labour force total}
#' \item{Underemployed total}
#' \item{Underutilised total}
#' \item{Monthly hours worked in all jobs}
#' \item{Monthly hours worked in all jobs (employed full-time)}
#' \item{Monthly hours worked in all jobs (employed part-time)}
#' \item{Not in the labour force (NILF)}
#' \item{Civilian population aged 15 years and over}
#' \item{Employment to population ratio}
#' \item{Unemployment rate}
#' \item{Unemployment rate looked for full-time work}
#' \item{Unemployment rate looked for only part-time work}
#' \item{Participation rate}
#' \item{Underemployment ratio (proportion of employed)}
#' \item{Underemployment rate (proportion of labour force)}
#' \item{Underutilisation rate}
#' }
#'
#' @param states string. Australia, or any State or Territory.
#' Multiple regions are allowed if sex and series_type are both NULL.
#'
#' @param years numeric. The year from which the plot should begin. The default is 2015
#' @param ages (option) string. Defaults to Total (age) which is the sum of all ages.
#' Supply an ABS age range to filter the indicator to only that age group, or multiple ages to compare across age groups.
#' ABS age ranges are:
#' \itemize{
#' \item{15-19 years}
#' \item{15-24 years}
#' \item{15-64 years}
#' \item{20-24 years}
#' \item{25-34 years}
#' \item{35-44 years}
#' \item{45-54 years}
#' \item{55 years and over}
#' \item{55-64 years}
#' \item{65 years and over}
#' \item{Total (age)}
#' }
#'
#' @param sex (optional) string. Defaults to Persons which is the sum of Males and Females.
#' Supply a gender to filter the indicator to only that age group, or multiple sex to compare across sex.
#' Applicable sex are:
#' \itemize{
#' \item{Males}
#' \item{Females}
#' \item{Persons}
#' }
#' @param series_types (optional) string. Defaults to Seasonally Adjusted. Supply a series_type to show only that series.
#' Available series_types are:
#' \itemize{
#' \item{Seasonally Adjusted}
#' \item{Original}
#' }
#' @param industries (optional) string. Defaults to Total (industry). Supply an industry to show only that series.
#' @param compare_aus (optional) logical. Defaults to TRUE which adds the Australian data for selected indicators.
#' @param plotly (optional) logical. Defaults to FALSE which creates a ggplot2 plot. Select TRUE to create a plotly plot.
#' Note that some aspects of the plot are unavailable if plotly = TRUE, including subtitles, and captions.
#' @param .data (optional). Specify a data frame or tibble object to use data other than the labour_force data
#' included in the `aitidata` package. You can use the pipe operator.
#' @param void (optional) logical. Defaults to FALSE. Specify TRUE to remove all plot elements except for the line.
#' @param markdown (optional) logical. Defaults to FALSE. Specify TRUE if you want to use markdown elements in
#' title/subtitle/axis titles. This requires the `ggtext` package to be installed.
#' @param palette string. The name of the colour palette to use for the plot. See `palette_names()`.
#' @param facet (optional) string. Defaults to NULL. Specify which variable to facet the graph on.

#'
#' @return A ggplot2 time-series plot or a plotly time-series plot if plotly = TRUE
#'
#' @name abs_plot
#'
#' @importFrom rlang .data
#' @export
#'
abs_plot <- function(.data = NULL,
                     indicator,
                     states,
                     years = 2015,
                     ages = "Total (age)",
                     industries = "Total (industry)",
                     sex = "Persons",
                     series_types = "Seasonally Adjusted",
                     compare_aus = TRUE,
                     markdown = FALSE,
                     palette = NULL,
                     facet = NULL,
                     plotly = FALSE,
                     void = FALSE) {


  #Error checking - only one variable is allowed to be of length > 1

  if (
    (length(ages) > 1 & length(states) > 1) & is.null(facet) |
    (length(sex) > 1 & length(states) > 1) & is.null(facet) |
    (length(ages) > 1 & length(sex) > 1) & is.null(facet)
  ) {

    guesses_facet <- dplyr::case_when(
      length(ages) > 1 ~ "age",
      length(sex) > 1 ~ "gender"
    )

    facet <- guesses_facet
    message(paste("You can't combine multiple states with multiple other variables without specifying a facet.\n
                  Setting facet =", guesses_facet))
  }

  #Indicators can not be compared

  if (length(indicator) != 1) {
    stop("More than one indicator requested. abs_plot can not compare indicators")
  }

  #Determine what is being plot

  if (length(states) == 1 & length(sex) > 1) {
    col_var <- "gender"
    n_cols <- length(sex)
    compare_aus <- FALSE
  } else if (length(states) == 1 & length(ages) > 1) {
    col_var <- "age"
    n_cols <- length(ages)
    compare_aus <- FALSE
  } else if (length(ages == 1) & length(sex) == 1) {
    col_var <- "state"
    n_cols <- length(states)
  }

  #Should Australia be added?
  if (compare_aus & !"Australia" %in% states) {
    states <- c(states, "Australia")
    n_cols <- n_cols + 1
  }

  if (is.null(.data)) {
    plot_data <- read_absdata("labour_force")
  } else if (is.data.frame(.data)) {
    plot_data <- .data
  }


  if (indicator %in% c("Monthly hours worked in all jobs (employed full-time)",
                       "Monthly hours worked in all jobs (employed part-time)",
                       "Employed part-time",
                       "Unemployed looked for full-time work",
                       "Unemployed looked for only part-time work",
                       "Unemployment rate looked for full-time work",
                       "Unemployment rate looked for only part-time work",
                       "Jobkeeper applications")) {
    series_types <- "Original"
  }


  #Make a couple of assumptions about the data - ie non labour force data is unlikely to have a gender or age dimension..?

  plot_data <- plot_data %>%
    dplyr::filter(.data$indicator == {{indicator}},
                  dplyr::if_any(dplyr::matches("gender"), ~ . %in% sex),
                  dplyr::if_any(dplyr::matches("series_type"), ~ . == series_types),
                  dplyr::if_any(dplyr::matches("age"), ~.x %in% ages),
                  dplyr::if_any(dplyr::matches("industry"), ~.x %in% industries),
                  dplyr::if_any(dplyr::matches("year"), ~.x >= years)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("state", "gender", "age", "series_type", "industry")))) %>%
    dplyr::mutate(index = 100 * .data$value / dplyr::first(x = .data$value, order_by = .data$date)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$state %in% states) %>%
    dplyr::mutate(state = factor(.data$state, levels = states)) %>%
    dplyr::arrange(.data$state)


  if (nrow(plot_data) == 0) {
    cli::cli_warn("Plot data is empty. Something has gone wrong! It's unlikely you'll get what you wanted.")
  }

  if (is.null(palette)) {
    palette <- Sys.getenv("R_REPORTABS_THEME", unset = NA)

    if (is.na(palette)) {
      palette <- "main"
    }

  } else if (is.character(palette)) {
    Sys.setenv(R_REPORTABS_THEME = palette)
  } else {
    palette <- "main"
  }

  plot_parameters <- plot_parameters(plot_data = plot_data,
                                     states = states,
                                     indicator = indicator,
                                     sex = sex,
                                     ages = ages,
                                     series_types = series_types,
                                     markdown = markdown,
                                     palette = palette,
                                     compare_aus = compare_aus,
                                     facet = facet)

  create_plot(plot_data, plot_parameters, void = void, plotly = plotly)

}

plot_parameters <- function(plot_data, states, indicator, sex = NULL, ages = NULL, series_types, markdown, palette, compare_aus, facet) {

  plot_parameters <- list()

  if (length(states) == 1 & length(sex) > 1) {
    plot_parameters$col_var <- "gender"
    plot_parameters$n_cols <- length(sex)
    plot_parameters$legend <- "bottom"
    compare_aus <- FALSE
  } else if (length(states) == 1 & length(ages) > 1) {
    plot_parameters$col_var <- "age"
    plot_parameters$n_cols <- length(ages)
    compare_aus <- FALSE
    plot_parameters$legend <- "bottom"
  } else if (length(ages) == 1 & length(sex) == 1) {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(states)
    plot_parameters$legend <- "none"
  } else {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(states)
    plot_parameters$legend <- "none"
  }



  if (compare_aus & !"Australia" %in% states) {
    states <- c(states, "Australia")
    plot_parameters$n_cols <- plot_parameters$n_cols + 1
  }

  to_match <- c("rate", "ratio", "proportion")

  if (any(grepl("payroll", indicator, ignore.case = TRUE))) {
    plot_parameters$index <- FALSE
    plot_parameters$subtitle <- "Index (Base: March 14 2020 = 100)"
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (length(states) >= 2 & !any(grepl(paste(to_match, collapse = "|"), indicator))) {
    plot_parameters$index <- TRUE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if ((length(states) == 1) & any(grepl(paste(to_match, collapse = "|"), indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_percent
  } else if (any(grepl(paste(to_match, collapse = "|"), indicator))) {
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
  plot_parameters$date_range <- c(min(plot_data$date), max(plot_data$date))
  plot_parameters$markdown <- markdown

  if (plot_parameters$markdown & plot_parameters$col_var == "state") {
    title_cols <- colorRampPalette(reportabs::aiti_palettes[[palette]])(plot_parameters$n_cols)
    names(title_cols) <- states

    plot_title_md <- paste0(": ", paste0("<span style = 'color:", title_cols, "'>", names(title_cols), "</span>", collapse = " and "))
    plot_subtitle_md <- ""

  } else if (plot_parameters$markdown & plot_parameters$col_var != "state") {

    subtitle_cols <- colorRampPalette(reportabs::aiti_palettes[[palette]])(plot_parameters$n_cols)

    tc <- c(length(sex), length(states), length(ages))
    names(tc) <- c("sex", "states", "ages")

    names(subtitle_cols) <- sym(names(which(tc == 2)))
    plot_subtitle_md <- paste0("<span style = 'color:", subtitle_cols, "'>", names(subtitle_cols), "</span>", collapse = " and ")
    plot_title_md <- paste0(": ",states)
  }

  else {
    plot_title_md <- if (plot_parameters$col_var == "state") "" else paste0(": ",states)
    plot_subtitle_md <- ""
  }
  if(plot_parameters$index) {
    plot_parameters$title <- paste0(stringr::str_to_title(indicator),  plot_title_md)
    plot_parameters$subtitle <- paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")
    plot_parameters$y_var <- "index"
    plot_parameters$legend <- if(markdown) "none" else "bottom"
  } else {
    plot_parameters$title <- if (markdown) paste0(stringr::str_to_title(indicator),  plot_title_md) else paste0(stringr::str_to_title(indicator),  plot_title_md)
    plot_parameters$subtitle <- if (markdown) plot_subtitle_md else ""
    plot_parameters$y_var <- "value"
    plot_parameters$legend <- if (markdown) "none" else "bottom"
  }

  if (!is.null(facet)) {
    plot_parameters$facet <- facet
  }

  plot_parameters$palette <- palette


  return(plot_parameters)
}



create_plot <- function(plot_data, plot_parameters, void, plotly) {

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
    ggplot2::scale_y_continuous(labels = plot_parameters$y_label) +
    scale_colour_aiti(palette = plot_parameters$palette)


  if (!void) {
    p <- p + ggplot2::labs(
      x = NULL,
      y = NULL,
      title = plot_parameters$title,
      subtitle = plot_parameters$subtitle,
      caption = plot_parameters$caption
    ) + ggplot2::guides(colour = ggplot2::guide_legend()) +
      theme_aiti(legend = plot_parameters$legend, markdown = plot_parameters$markdown)

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
      theme_aiti(legend = "bottom")

    p <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(autosize = TRUE,
                     font = if (plot_parameters$palette == "legacy") "Roboto" else "Space Mono",
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







