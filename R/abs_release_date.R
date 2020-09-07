#' Return the date that the most current
#' version of an ABS Time Series was released
#'
#' @param cat_no string. Include the .0
#' @export

abs_current_release <- function(cat_no) {

  release_url <- glue::glue("https://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno={cat_no}&&tabname=Past%20Future%20Issues")

  release_page <- xml2::read_html(release_url)

  release_table <- tibble::tibble(release = release_page %>%  rvest::html_nodes("#mainpane a") %>% rvest::html_text(),
                                   url_suffix = release_page %>%  rvest::html_nodes("#mainpane a") %>% rvest::html_attr("href"))

  release_date <- release_table %>%
    dplyr::filter(grepl("(Latest)", .data$release)) %>%
    dplyr::pull(.data$release) %>%
    stringr::str_remove(" \\(Latest\\)") %>%
    stringr::str_extract("Week ending \\d+\\s{1}\\w+ \\d+$|(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?).*") %>%
    stringr::str_replace_all(" ", "%20")

  download_url <- glue::glue("https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/{cat_no}{release_date}?OpenDocument")

  cur_release <- xml2::read_html(download_url) %>%
    rvest::html_nodes(xpath = '//*[@id="Release"]') %>%
    rvest::html_text() %>%
    stringr::str_extract("[0-9/]{8,}")

  cur_release <- as.Date(cur_release, format = "%d/%m/%y")


  return(cur_release)

}

#' Next Release date of an ABS Time Series
#'
#' @param cat_no string. include the .0
#'
#' @export

abs_next_release <- function(cat_no) {

  release_url <- glue::glue("https://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno={cat_no}&&tabname=Past%20Future%20Issues")

  release_page <- xml2::read_html(release_url)

  release_date <- release_page %>%
    rvest::html_nodes(xpath = '//*[@id="mainpane"]/div/ul[1]/li') %>%
    rvest::html_text()

  next_date <- stringr::str_sub(release_date, start = -10)

  next_date <- as.Date(next_date, format = "%d/%m/%Y")

  return(next_date)

}

#' Print the most recent month/year of an ABS Time Series
#'
#' @param data the abs time series data obtained via readabs
#' @param plus add or subtract a year or month (depending on parameter ym) from the
#' @param ym 'year' for the release year and 'month' for the release month.
#'
#' @return a string
#' @importFrom dplyr filter pull distinct
#' @importFrom magrittr %>%
#' @importFrom lubridate month year
#' @export release
#'
#' @examples \dontrun{release(labour_force)}
release <- function(data = NULL, ym = "year", plus = 0L) {

  if (ym == "year") {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::year(release) + plus

  } else if (ym == "month") {
    release <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date)

    release <- lubridate::month(release + months(plus), abbr = FALSE, label = TRUE)

  }

  return(release)
}
