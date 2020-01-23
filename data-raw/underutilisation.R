## code to prepare `underutilisation` dataset goes here
library(readabs)
library(tidyverse)

if(!file.exists("data-raw/underutilisation.csv")) {

  read_abs(cat_no = "6202.0", tables = 22, retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation_age.csv")

  read_abs(cat_no = "6202.0", tables = 23, retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation_state.csv")

}

raw_age <- readr::read_csv("data-raw/underutilisation_age.csv")

underutilisation_age <- raw_age %>%
  tidyr::separate(series, into = c("indicator", "gender", "age"), sep = ";") %>%
  dplyr::mutate(age = ifelse(age == "", "Total (age)", age),
                value = ifelse(unit == "000", (1000*value), (value)),
                year = lubridate::year(date),
                month = lubridate::month(date, label = T, abbr = F),
                state = "Australia") %>%
  dplyr::mutate_at(.vars = c("indicator", "gender", "age"), base::trimws) %>%
  dplyr::mutate_at(.vars = c("indicator","gender"), ~stringr::str_remove_all(., "> ")) %>%
  dplyr::distinct()

raw_state <- readr::read_csv("data-raw/underutilisation_state.csv")

underutilisation_state <- raw_state %>%
  tidyr::separate(series, into = c("indicator", "gender", "state"), sep = ";") %>%
  dplyr::mutate(state = ifelse(state == "", "Australia", state),
                value = ifelse(unit == "000", (1000*value), (value)),
                year = lubridate::year(date),
                month = lubridate::month(date, label = T, abbr = F),
                age = "Total (age)") %>%
  dplyr::mutate_at(.vars = c("indicator", "gender", "state"), trimws) %>%
  dplyr::mutate_at(.vars = c("indicator", "gender", "state"), ~stringr::str_remove_all(., "> ")) %>%
  dplyr::distinct()

underutilisation <- dplyr::bind_rows(underutilisation_age, underutilisation_state) %>%
  dplyr::distinct()

write_csv(underutilisation, "data-raw/underutilisation.csv")

usethis::use_data(underutilisation, overwrite = TRUE, compress = "xz")
