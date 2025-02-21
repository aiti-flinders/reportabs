#' ABS Labour Force and Hours Worked data
#'
#' A subset of data from the ABS Labour Force release,
#' capturing seasonally adjusted hours worked and labour force status
#' data for the past 5 years.
#'
#' @format ## `labour_force_briefing`
#' A data frame with 23,180 rows and 6 columns:
#' \describe{
#'  \item{date}{Date}
#'  \item{sex}{Sex}
#'  \item{state}{State name}
#'  \item{series_type}{Series Type}
#'  \item{indicator}{Labour force indicator.
#'  Note that Employed part-time is derived for the states,
#'  and Underutilised total is derived for the states and Australia}
#'  \item{value}{Value}
#'  \item{unit}{The indicator's units}
#' }
#' @source <https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release>
"labour_force_briefing"
