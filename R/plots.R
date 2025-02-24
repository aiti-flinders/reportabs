#' Convenience functions for creating charts for ABS and other
#' @description These functions are wrap-around functions for abs_plot() for most of the
#' indicators you should ever need to plot. For more information about what options you can specify see
#' \code{\link{abs_plot}}
#'
#' @param states The states to include in the plot.
#' @param ... Other options passed to abs_plot
#'
#' @export
#'
plot_employed_total <- function(states, ...) {
  abs_plot(over = list(state = states, indicator = "Employed total"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_employed_full_time <- function(states, ...) {
  abs_plot(over = list(state = states, indicator = "Employed full-time"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_employed_part_time <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Employed part-time"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployed_total <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployed total"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployed_looked_for_full_time_work <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployed looked for full-time work"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployed_looked_for_part_time_work <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployed looked for only part-time work"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_labour_force_total <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Labour force total"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_underemployed_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Underemployed total", ...)
}

#' @export
#' @rdname plot_employed_total
plot_underutilised_total <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Underutilised total"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_hours_worked <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Monthly hours worked in all jobs"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_hours_worked_full_time <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Monthly hours worked in all jobs (employed full-time)"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_hours_worked_part_time <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Monthly hours worked in all jobs (employed part-time)", ...))
}

#' @export
#' @rdname plot_employed_total
plot_not_in_the_labour_force <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Not in the labour force (NILF)"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_working_population <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Civilian population aged 15 years and over"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_employment_population_ratio <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Employment to population ratio"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployment_rate <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployment rate"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployment_rate_looked_for_full_time_work <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployment rate looked for full-time work"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_unemployment_rate_looked_for_part_time_work <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Unemployment rate looked for only part-time work"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_participation_rate <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Participation rate"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_underemployment_ratio <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Underemployment ratio (proportion of employed)"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_underemployment_rate <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Underemployment rate (proportion of labour force)"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_underutilisation_rate <- function(states, ...) {
  abs_plot(over = list(states = states, indicator = "Underutilisation rate"), ...)
}

#' @export
#' @rdname plot_employed_total
plot_sahm_recession_indicator <- function(states, ...) {
  sahm(state = states, ...)
}


