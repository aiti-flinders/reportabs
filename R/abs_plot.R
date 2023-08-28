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
#' @param state string. Australia, or any State or Territory.
#' Multiple regions are allowed if sex and series_type are both NULL.
#'
#' @param years numeric. The year from which the plot should begin. The default is 2015
#' @param age (option) string. Defaults to Total (age) which is the sum of all ages.
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
#' @param series_type (optional) string. Defaults to Seasonally Adjusted. Supply a series_type to show only that series.
#' Available series_type are:
#' \itemize{
#' \item{Seasonally Adjusted}
#' \item{Original}
#' }
#' @param type one of "bar" for a bar chart, or "line" for a time-series chart.
#' @param industry (optional) string. Defaults to Total (industry). Supply an industry to show only that series.
#' @param compare_aus (optional) logical. Defaults to TRUE which adds the Australian data for selected indicators.
#' @param plotly (optional) logical. Defaults to FALSE which creates a ggplot2 plot. Select TRUE to create a plotly plot.
#' Note that some aspects of the plot are unavailable if plotly = TRUE, including subtitles, and captions.
#' @param data (optional). Specify a data frame or tibble object to use data other than the labour_force data
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
#' @importFrom stats reorder
#' @export
#'
abs_plot <- function(data,
                     indicator,
                     state,
                     years = 2015,
                     age = "Total (age)",
                     industry = "Total (industry)",
                     sex = "Persons",
                     series_type = "Seasonally Adjusted",
                     type,
                     compare_aus = TRUE,
                     markdown = FALSE,
                     plotly = FALSE,
                     void = FALSE,
                     palette = NULL,
                     facet = NULL) {




  #Error checking - only one variable is allowed to be of length > 1



  #Indicators can not be compared

  if (length(indicator) != 1) {
    stop("More than one indicator requested. abs_plot can not compare indicators")
  }


  if (compare_aus & !"Australia" %in% state & type == "line") {
    state <- c(state, "Australia")
  }

  if (
    (length(age) > 1 & length(state) > 1) & is.null(facet) |
    (length(sex) > 1 & length(state) > 1) & is.null(facet) |
    (length(age) > 1 & length(sex) > 1) & is.null(facet)
  ) {

    guesses_facet <- dplyr::case_when(
      length(ages) > 1 ~ "age",
      length(sex) > 1 ~ "gender",
      length(industry) > 1 ~ "industry"
    )

    facet <- guesses_facet
    message(paste("You can't combine multiple states with multiple other variables without specifying a facet.\n
                  Setting facet =", guesses_facet))
  }

  if (indicator %in% c("Monthly hours worked in all jobs (employed full-time)",
                       "Monthly hours worked in all jobs (employed part-time)",
                       "Employed part-time",
                       "Unemployed looked for full-time work",
                       "Unemployed looked for only part-time work",
                       "Unemployment rate looked for full-time work",
                       "Unemployment rate looked for only part-time work",
                       "Jobkeeper applications") | "industry" %in% colnames(data)) {
    series_type <- "Original"
  }


  #Make a couple of assumptions about the data - ie non labour force data is unlikely to have a gender or age dimension..?

  if (type == "bar") {
    plot_data <- data |>
      dplyr::filter(.data$indicator == {{indicator}},
                    dplyr::if_any(dplyr::matches("gender"), \(x) x %in% sex),
                    dplyr::if_any(dplyr::matches("series_type"), \(x) x == {{series_type}}),
                    dplyr::if_any(dplyr::matches("age"), \(x) x %in% {{age}}),
                    .data$industry != {{industry}},
                    .data$state %in% {{state}},
                    .data$date == max(data$date))
  } else {

    plot_data <- data %>%
      dplyr::filter(.data$indicator == {{indicator}},
                    dplyr::if_any(dplyr::matches("gender"), \(x) x %in% sex),
                    dplyr::if_any(dplyr::matches("series_type"), \(x) x == {{series_type}}),
                    dplyr::if_any(dplyr::matches("age"), \(x) x %in% {{age}}),
                    dplyr::if_any(dplyr::matches("industry"), \(x) x %in% {{industry}}),
                    dplyr::if_any(dplyr::matches("year"), ~.x >= years)) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("state", "gender", "age", "series_type", "industry")))) %>%
      dplyr::mutate(index = 100 * .data$value / dplyr::first(x = .data$value, order_by = .data$date)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$state %in% {{state}}) %>%
      dplyr::mutate(state = factor(.data$state, levels = {{state}})) %>%
      dplyr::arrange(.data$state)
  }


  if (nrow(plot_data) == 0) {
    cli::cli_abort("Plot data is empty. Something has gone wrong! It's unlikely you'll get what you wanted.")
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
                                     type = type,
                                     state = state,
                                     indicator = indicator,
                                     industry = industry,
                                     sex = sex,
                                     age = age,
                                     series_type = series_type,
                                     markdown = markdown,
                                     palette = palette,
                                     compare_aus = compare_aus,
                                     facet = facet)

  create_plot(plot_data, plot_parameters, void = void, plotly = plotly)

}

plot_parameters <- function(plot_data, type, state, indicator, industry = NULL, sex = NULL, age = NULL, series_type, markdown, palette, compare_aus, facet) {

  plot_parameters <- list()


  plot_parameters$compare_aus <- compare_aus



  # Determine what is being plot.
  # We're interested in both "within" - i.e. male vs female unemployment rates in South Australia and "across" - i.e. unemployment rates in South Australia and Australia.
  # For both "within" and "across", i.e. male vs female unemployment rates in South Australia and Australia, either use a facet, or it's an error.
  # It gets a bit trickier with industry because a single industry plot is of interest where a single gender plot isn't that interesting (or should it be?)
  # In that case, the approach below using the length of each vector isn't appropriate.


  if (length(state) == 1 & any(age == "Total (age)") & any(!sex == "Persons") & any(industry == "Total (industry)")) {
    plot_parameters$col_var <- "sex"
    plot_parameters$n_cols <- length(sex)
    plot_parameters$compare_aus <- FALSE
  } else if (length(state) == 1 & any(!age == "Total (age)") & any(sex == "Persons") & any(industry == "Total (industry)")) {
    plot_parameters$col_var <- "age"
    plot_parameters$n_cols <- length(age)
    plot_parameters$compare_aus <- FALSE
  } else if (length(state) == 1 & any(age == "Total (age)") & any(sex == "Persons") & any(!industry == "Total (industry)")) {
    plot_parameters$col_var <- "industry"
    plot_parameters$n_cols <- length(industry)
    plot_parameters$compare_aus <- FALSE
  } else if (any(age == "Total (age)") & any(sex == "Persons") & any(industry == "Total (industry)")) {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(state)
  } else if (length(state) > 1) {
    plot_parameters$col_var <- "state"
    plot_parameters$n_cols <- length(state)
  }


  if (type == "bar") {
    plot_parameters$plot_type <- "bar"
    plot_parameters$compare_aus <- FALSE
    plot_parameters$n_cols <- 1


  } else {
    plot_parameters$plot_type <- "line"
  }


  if (plot_parameters$compare_aus & !"Australia" %in% state & plot_parameters$plot_type != "bar") {
    state <- c(state, "Australia")
    plot_parameters$n_cols <- plot_parameters$n_cols + 1
  }

  to_match <- c("rate", "ratio", "proportion")

  if (any(grepl("payroll", indicator, ignore.case = TRUE))) {
    plot_parameters$index <- FALSE
    plot_parameters$subtitle <- "Index (Base: March 14 2020 = 100)"
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (length(state) >= 2 & !any(grepl(paste(to_match, collapse = "|"), indicator))) {
    plot_parameters$index <- TRUE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if ((length(state) == 1) & any(grepl(paste(to_match, collapse = "|"), indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_percent
  } else if (any(grepl(paste(to_match, collapse = "|"), indicator))) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::percent_format(scale = 1)
    plot_parameters$hover <- as_comma
  } else if (any(industry == "Total (industry)")) {
    plot_parameters$index <- FALSE
    plot_parameters$y_label <- scales::comma_format(scale = 1)
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
    industry != "Total (industry)" ~ "4",
    TRUE ~ "12"
  )

  series_type <- unique(series_type)


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
    industry != "Total (industry)" ~ paste0("Source: ABS Labour Force, Australia, Detailed. ",
                                              reportabs::release(plot_data, "month"),
                                              " ",
                                              reportabs::release(plot_data, "year"),
                                              " (Table ", table_no, ", ", series_type, ")"),
    TRUE ~ paste0("Source: ABS Labour Force, Australia, ",
                  reportabs::release(plot_data, "month"),
                  " ",
                  reportabs::release(plot_data, "year"),
                  " (Table ", table_no, ", ", series_type, ")")
  )


  plot_parameters$num_months <- as.numeric(max(plot_data$month))
  plot_parameters$month <- lubridate::month(min(plot_data$date), abbr = FALSE, label = TRUE)
  plot_parameters$year <- lubridate::year(min(plot_data$date))
  plot_parameters$caption <- caption_table
  plot_parameters$date_range <- c(min(plot_data$date), max(plot_data$date))
  plot_parameters$markdown <- markdown


  if (plot_parameters$plot_type == "line") {

    # Generate some coloured titles and subtitles for markdown elements

      title_cols <- colorRampPalette(reportabs::aiti_palettes[[palette]])(plot_parameters$n_cols)
      names(title_cols) <- eval(sym(plot_parameters$col_var))
      plot_title_md <- paste0(stringr::str_to_title(indicator), paste0(": ", paste0("<span style = 'color:", title_cols, "'>", names(title_cols), "</span>", collapse = " and ")))
      plot_subtitle_md <- ""

      subtitle_cols <- colorRampPalette(reportabs::aiti_palettes[[palette]])(plot_parameters$n_cols)
      names(subtitle_cols) <- sort(eval(sym(plot_parameters$col_var)))
      plot_subtitle_md <- paste0("<span style = 'color:", subtitle_cols, "'>", names(subtitle_cols), "</span>", collapse = " and ")



    # Rules for (line) plots:
    # With a single state, put the name of the state into the title, and don't show the legend
    # (unless comparing across another variable)

    if (length(state) == 1 & length(age) == 1 & length(industry) == 1 & length(sex) == 1) {
      plot_parameters$title <- if(markdown) plot_title_md else {paste0(stringr::str_to_title(indicator), ": ", state)}
      plot_parameters$legend = "none"
      plot_parameters$y_var = "value"
    } else if (length(state) == 1 & (length(age) > 1 | length(industry) > 1 | length(sex) > 1)) {
      plot_parameters$title <- paste0(stringr::str_to_title(indicator), ": ", state)
      plot_parameters$subtitle <- if(markdown) plot_subtitle_md else NULL
      plot_parameters$legend <- if (markdown) "none" else "bottom"
      plot_parameters$y_var = "value"
    } else if (length(state) > 1) {
      plot_parameters$title <- if(markdown) plot_title_md else {stringr::str_to_title(indicator)}
      plot_parameters$legend = if(markdown) "none" else "bottom"
      plot_parameters$y_var = if(plot_parameters$index) "index" else "value"
    }
  }

  if (plot_parameters$plot_type == "bar") {
    plot_parameters$y_var = "value"
    plot_parameters$title = paste0(stringr::str_to_title(indicator), ": ", state)
  }

  # Payroll data (and some others, like price indexes if I ever get around to it) are already indexed

  is_payroll <- grepl("payroll", indicator, ignore.case = TRUE)
  is_cpi <- grepl("cpi", indicator, ignore.case = TRUE)

  if (plot_parameters$index | is_payroll & plot_parameters$plot_type == "line") {
    plot_parameters$y_var <- "index"
    plot_parameters$subtitle <- if(is_payroll) "Index (Base: March 14 2020 = 100)" else paste("Index (Base:", plot_parameters$month, plot_parameters$year, "= 100)")

  }

  # Is a legend needed?
  plot_parameters$legend <- dplyr::case_when(
    markdown & is_payroll & is.null(facet) ~ "bottom",
    markdown ~ "none",
    length(age) > 1 | length(sex) > 1 | length(industry) > 1 | length(state) > 1 ~ "bottom",
    TRUE ~ "none"

  )
  #else if (plot_parameters$plot_type == "bar" & !grepl("payroll", indicator, ignore.case = TRUE)) {
  #   plot_parameters$legend <- "none"
  #   plot_parameters$subtitle <- states
  #   plot_parameters$title <- paste0(stringr::str_to_title(indicator))
  #   plot_parameters$y_var <- "value"
  # } else {
  #   plot_parameters$title <- if (markdown) paste0(stringr::str_to_title(indicator),  plot_title_md) else paste0(stringr::str_to_title(indicator),  plot_title_md)
  #   plot_parameters$subtitle <- if (markdown) plot_subtitle_md else plot_parameters$subtitle
  #   plot_parameters$y_var <- "value"
  #   plot_parameters$legend <- if (markdown) "none" else "bottom"
  # }

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

  if (plot_parameters$plot_type == "bar") {

    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = value,
                                      y = reorder(industry, .data[[plot_parameters$y_var]]),
                                      fill = .data[[plot_parameters$col_var]])) +
      ggplot2::geom_col(alpha = 0.3) +
      ggplot2::scale_x_continuous(labels = scales::comma_format()) +
      ggplot2::geom_text(ggplot2::aes(x = 0, label = industry), hjust = 0) +
      ggplot2::labs(x = NULL,
                    y = NULL,
                    title = plot_parameters$title,
                    subtitle = plot_parameters$subtitle,
                    caption = plot_parameters$caption) +
      theme_aiti(flipped = TRUE,
                 legend = plot_parameters$legend,
                 markdown = plot_parameters$markdown) +
      theme(axis.text.y = element_blank()) +
      scale_fill_aiti(palette = plot_parameters$palette)
  } else {

    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = date,
                                      y = .data[[plot_parameters$y_var]],
                                      colour = .data[[plot_parameters$col_var]])) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_date(date_labels = "%e %b\n%Y",
                            breaks = date_breaks) +
      ggplot2::scale_y_continuous(labels = plot_parameters$y_label) +
      scale_colour_aiti(palette = plot_parameters$palette)  +
      ggplot2::labs(x = NULL,
                    y = NULL,
                    title = plot_parameters$title,
                    subtitle = plot_parameters$subtitle,
                    caption = plot_parameters$caption) +
      ggplot2::guides(colour = ggplot2::guide_legend()) +
      theme_aiti(legend = plot_parameters$legend, markdown = plot_parameters$markdown)

  }


  if (void) {
    p <- p + ggplot2::theme_void() + ggplot2::theme(legend.position = "none")}

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

abs_plot_line <- function(data, indicator, state, type = "line", ...) {

  abs_plot(data = data, indicator = indicator, state = state, type = "line", ...)

}

abs_plot_bar <- function(data, indicator, state, type = "bar", ...) {

  abs_plot(data = data, indicator = indicator, state = state, type = "bar", ...)


}








