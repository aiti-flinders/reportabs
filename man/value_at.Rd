% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/temporal_reporting.R
\name{value_at}
\alias{value_at}
\title{Value of an ABS Time Series indicator for a specific year and month.}
\usage{
value_at(data, filter_with, at_year = NULL, at_month = NULL)
}
\arguments{
\item{data}{a dataframe of ABS time series data.}

\item{filter_with}{a named list of at least an indicator to filter the dataframe on.}

\item{at_year}{a year (numeric). Defaults to the most recent year if NULL (the default) and at_month = NULL.
If only at_year is specified, the value is averaged over the year (Not yet implemented)}

\item{at_month}{a month (name). Defaults to the most recent month if NULL (the default)}
}
\value{
a number
}
\description{
Value of an ABS Time Series indicator for a specific year and month.
}
\examples{
library(reportabs)
labour_force_briefing <- read_absdata("labour_force_briefing")
value_at(labour_force_briefing , filter_with = list(indicator = "Employed total"))

}
