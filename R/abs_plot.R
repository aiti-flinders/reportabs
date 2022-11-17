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
#' title/subtitle/axis titles
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
    warning("Plot data is empty. Something has gone wrong!")
  }

  plot_parameters <- plot_parameters(plot_data = plot_data,
                                     states = states,
                                     indicator = indicator,
                                     sex = sex,
                                     ages = ages,
                                     series_types = series_types,
                                     markdown = markdown,
                                     compare_aus = compare_aus,
                                     facet = facet)

  create_plot(plot_data, plot_parameters, void = void, plotly = plotly)

}






