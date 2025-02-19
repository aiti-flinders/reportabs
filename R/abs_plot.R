#' Generate a time-series plot for ABS labour force indicators.
#'
#' \code{abs_plot()} can automatically generate appropriate plots for ABS
#' Time Series indicators for both static display in documents, or RMarkdown,
#' as well as interactive plots through plotly.
#'
#' @param over named list specifying what should be plot.
#' @param years numeric.
#' @param compare_aus (optional) logical. Defaults to TRUE which adds the Australian data for selected indicators.
#' @param plotly (optional) logical. Defaults to FALSE which creates a ggplot2 plot. Select TRUE to create a plotly plot.
#' Note that some aspects of the plot are unavailable if plotly = TRUE, including subtitles, and captions.
#' @param data (optional). Specify a data frame or tibble object to use data other than the labour_force data
#' included in the `aitidata` package. You can use the pipe operator.
#' @param void (optional) logical. Defaults to FALSE. Specify TRUE to remove all plot elements except for the line.
#' @param markdown (optional) logical. Defaults to FALSE. Specify TRUE if you want to use markdown elements in
#' title/subtitle/axis titles
#' @param facet (optional) string. Defaults to NULL. Specify which variable to facet the graph on.
#' @param ... other arguments passed to theme_aiti()
#'
#' @return A ggplot2 time-series plot or a plotly time-series plot if plotly = TRUE
#'
#' @name abs_plot
#'
#' @importFrom rlang .data
#' @export
#'
abs_plot <- function(data = NULL,
                     over,
                     years = 2015,
                     compare_aus = TRUE,
                     markdown = FALSE,
                     facet = NULL,
                     plotly = FALSE,
                     void = FALSE,
                     ...) {

  if (is.null(data)) {
    plot_data <- read_absdata("labour_force")
  } else if (is.data.frame(data)) {
    plot_data <- data
  }


  over <- make_safe(plot_data, over)

  if (compare_aus && !"Australia" %in% over$state) {
    over$state <- c(over$state, "Australia")
  }

  #Error checking - only one variable is allowed to be of length > 1


  e <- Map(length, over)

  if (sum(e > 1) > 1) {
    stop("Only one variable in argument `over` is allowed to be of length greater than 1")
  }




  # if (
  #   (length(ages) > 1 & length(states) > 1) & is.null(facet) |
  #   (length(sex) > 1 & length(states) > 1) & is.null(facet) |
  #   (length(ages) > 1 & length(sex) > 1) & is.null(facet)
  #   ) {
  #
  #   guesses_facet <- dplyr::case_when(
  #     length(ages) > 1 ~ "age",
  #     length(sex) > 1 ~ "sex"
  #   )
  #
  #   facet <- guesses_facet
  #   message(paste("You can't combine multiple states with multiple other variables without specifying a facet.\n
  #                 Setting facet =", guesses_facet))
  # }



  #Determine what is being plot. If something has length greater than 1, its that
  col_var <- ifelse(length(names(e[e > 1])) == 0, "indicator", names(e[e > 1]))
  n_cols <- if (length(col_var) == 0) {1} else {e[[col_var]]}

  if ("state" %in% names(over) && !"Australia" %in% over$state && all(e == 1) && compare_aus) {
    over$state = c(over$state, "Australia")
    n_cols <- n_cols + 1
  } else {
    compare_aus <- FALSE
  }



  plot_data <- plot_data |>
    filter_list(over) |>
    dplyr::filter(lubridate::year(date) >= years) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("state", "sex", "age", "indicator", "series_type", "industry")))) |>
    dplyr::mutate(index = 100 * .data$value / dplyr::first(x = .data$value, order_by = .data$date)) |>
    dplyr::ungroup()

  if (nrow(plot_data) == 0) {
    warning("Plot data is empty. Something has gone wrong!")
  }

  plot_parameters <- plot_parameters(plot_data = plot_data,
                                     over,
                                     col_var = col_var,
                                     n_cols = n_cols,
                                     markdown = markdown,
                                     compare_aus = compare_aus,
                                     facet = facet)

  #return(list(pp = plot_parameters, pd = plot_data))
  create_plot(plot_data, plot_parameters, void = void, plotly = plotly, ...)

}






