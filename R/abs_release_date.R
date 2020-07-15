#' Next Release of an ABS Time Series
#'
#' @param cat_no string. Include the .0
#'

abs_next_release <- function(cat_no) {

  release_url <- glue::glue("https://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno={cat_no}&&tabname=Past%20Future%20Issues")

  release_page <- xml2::read_html(release_url)

  release_date <- release_page %>%
    rvest::html_nodes(xpath = '//*[@id="mainpane"]/div/ul[1]/li') %>%
    rvest::html_text()

  next_release <- stringr::str_sub(release_date, start = 13, end = 20)

  next_release <- zoo::as.yearmon(next_release, format = "%b %Y")

  return(next_release)

}

#' Next Release date of an ABS Time Series
#'
#' @param cat_no string. include the .0

abs_release_date <- function(cat_no) {

  release_url <- glue::glue("https://www.abs.gov.au/AUSSTATS/abs@.nsf/second+level+view?ReadForm&prodno={cat_no}&&tabname=Past%20Future%20Issues")

  release_page <- xml2::read_html(release_url)

  release_date <- release_page %>%
    rvest::html_nodes(xpath = '//*[@id="mainpane"]/div/ul[1]/li') %>%
    rvest::html_text()

  next_date <- stringr::str_sub(release_date, start = -10)

  next_date <- as.Date(next_date, format = "%d/%m/%Y")

  return(next_date)

}
