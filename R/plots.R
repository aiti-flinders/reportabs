plot_employed_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Employed total", ...)
}

plot_employed_full_time <- function(states, ...) {
  abs_plot(states = states, indicator = "Employed full-time", ...)
}

plot_employed_part_time <- function(states, ...) {
  abs_plot(states = states, indicator = "Employed part-time", ...)
}

plot_unemployed_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployed total", ...)
}

plot_unemployed_looked_for_full_time_work <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployed looked for full-time work", ...)
}

plot_unemployed_looked_for_part_time_work <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployed looked for only part-time work", ...)
}

plot_labour_force_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Labour force total", ...)
}

plot_underemployed_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Underemployed total", ...)
}

plot_underutilised_total <- function(states, ...) {
  abs_plot(states = states, indicator = "Underutilised total", ...)
}

plot_hours_worked <- function(states, ...) {
  abs_plot(indicator = "Monthly hours worked in all jobs", ...)
}

plot_hours_worked_full_time <- function(states, ...) {
  abs_plot(states = states, indicator = "Monthly hours worked in all jobs (employed full-time)", ...)
}

plot_hours_worked_part_time <- function(states, ...) {
  abs_plot(states = states, indicator = "Monthly hours worked in all jobs (employed part-time)", ...)
}

plot_not_in_the_labour_force <- function(states, ...) {
  abs_plot(states = states, indicator = "Not in the labour force (NILF)", ...)
}

plot_working_population <- function(states, ...) {
  abs_plot(states = states, indicator = "Civilian population aged 15 years and over", ...)
}

plot_employment_population_ratio <- function(states, ...) {
  abs_plot(states = states, indicator = "Employment to population ratio", ...)
}

plot_unemployment_rate <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployment rate", ...)
}

plot_unemployment_rate_looked_for_full_time_work <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployment rate looked for full-time work", ...)
}

plot_unemployment_rate_looked_for_part_time_work <- function(states, ...) {
  abs_plot(states = states, indicator = "Unemployment rate looked for only part-time work", ...)
}

plot_participation_rate <- function(states, ...) {
  abs_plot(states = states, indicator = "Participation rate", ...)
}

plot_underemployment_ratio <- function(states, ...) {
  abs_plot(states = states, indicator = "Underemployment ratio (proportion of employed)", ...)
}

plot_underemployment_rate <- function(states, ...) {
  abs_plot(states = states, indicator = "Underemployment rate (proportion of labour force)", ...)
}

plot_underutilisation_rate <- function(states, ...) {
  abs_plot(states = states, indicator = "Underutilisation rate", ...)
}

plot_sahm_recession_indicator <- function(states, ...) {
  sahm(region = states, ...)
}

plot_jobkeeper_applications <- function(states) {

}
