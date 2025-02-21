#' Apply dominance check to ABS unit record data. This function is not exported
#'
#' @param data A data frame
#' @param summary_var The variables needing output clearance
#' @param by group by
#'
#' @returns data frame

clear_output <- function(data, summary_var, by) {

  dc <- data |>
    dplyr::group_by(dplyr::pick({{by}})) |>
    dplyr::summarise(dplyr::across({{summary_var}},
                     list(dc_150 = ~dc150(.x),
                          dc_267 = ~dc267(.x))),
              .groups = "drop")
  dc

}

dc150 <- function(x) {
  max(x, na.rm = TRUE) /sum(x, na.rm = TRUE)
}

dc267 <- function(x) {
  (max(x, na.rm = TRUE) + sort(unique(x), TRUE)[2]) / sum(x, na.rm = TRUE)
}
