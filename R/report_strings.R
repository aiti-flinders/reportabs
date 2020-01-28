#' Title
#'
#' @param data
#' @param period
#' @param type
#' @param ym
#'
#' @return
#' @export change
#'
#' @examples
change <- function(
  data = .data,
  filter_with = NULL,
  type = 'id',
  ym = 'year'
) {
  if(ym == "year") {
    value_1 <- round(value_at(data, filter_with, at_year = 2019, at_month = "January"),1)
    value_2 <- round(value_at(data, filter_with, at_year = 2018, at_month = "January"),1)
  } else if(ym == "month") {
    value_1 <- round(value_at_month(chunk_indicator, at_month = release_month(plus = period[1])),1)
    value_2 <- round(value_at_month(chunk_indicator, at_month = release_month(plus = period[2])),1)
  }

  if(type == "id") {
    dplyr::case_when(value_1 > value_2 ~ 'increased by',
      value_1 < value_2 ~ 'decreased by',
      value_1 == value_2 ~ 'remained steady at')
  } else if(type == "ab") {
    dplyr::case_when(value_1 > value_2 ~ 'above',
      value_1 < value_2 ~ 'below',
      value_1 == value_2 ~ 'the same as')
  } else if(type == 'rf') {
    dplyr::case_when(value_1 > value_2 ~ 'risen',
      value_1 < value_2 ~ 'fallen',
      value_1 == value_2 ~ 'remained steady at')
  } else if(type == 'present') {
    dplyr::case_when(value_1 > value_2 ~ 'an increase',
      value_1 < value_2 ~ 'a decrease',
      value_1 == value_2 ~ str_c("the same level as last", ym))
  }
}


