## code to prepare `underutilisation` dataset goes here
library(readabs)
library(tidyverse)

force_update <- TRUE

any_file_missing <- !file.exists("data-raw/underutilisation_age.csv") |
  !file.exists("data-raw/underutilisation_state.csv") |
  !file.exists("data-raw/underutilisation_volume.csv")

if(any_file_missing | force_update) {

  read_abs(cat_no = "6291.0.55.003", tables = '23a', retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation_volume.csv")

  read_abs(cat_no = "6202.0", tables = 22, retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation_age.csv")

  read_abs(cat_no = "6202.0", tables = 23, retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation_state.csv")

}

raw_age <- read_csv("data-raw/underutilisation_age.csv")

underutilisation_age <- raw_age %>%
  separate(series, into = c("indicator", "gender", "age"), sep = ";") %>%
  mutate(age = ifelse(age == "", "Total (age)", age),
                value = ifelse(unit == "000", (1000*value), value),
                year = lubridate::year(date),
                month = lubridate::month(date, label = T, abbr = F),
                state = "Australia") %>%
  mutate_at(.vars = c("indicator", "gender", "age"), trimws) %>%
  mutate_at(.vars = c("indicator","gender"), ~str_remove_all(., "> ")) %>%
  select(date, year, month, indicator, gender, age, state, series_type, value) %>%
  distinct()

raw_state <- read_csv("data-raw/underutilisation_state.csv")

underutilisation_state <- raw_state %>%
  separate(series, into = c("indicator", "gender", "state"), sep = ";") %>%
  mutate(state = ifelse(state == "", "Australia", state),
    value = ifelse(unit == "000", (1000*value), value),
    year = lubridate::year(date),
    month = lubridate::month(date, label = T, abbr = F),
    age = "Total (age)") %>%
  mutate_at(.vars = c("indicator", "gender", "state"), trimws) %>%
  mutate_at(.vars = c("indicator", "gender", "state"), ~str_remove_all(., "> ")) %>%
  select(date, year, month, indicator, gender, age, state, series_type, value) %>%
  distinct()

raw_volume <- read_csv("data-raw/underutilisation_volume.csv")

underutilisation_volume <- raw_volume %>%
  separate(series, into = c("state", "indicator", "gender"), sep = ";", extra = "drop") %>%
  mutate(age = "Total (age)",
    value = ifelse(unit == "000", (1000*value), value),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  mutate_at(.vars = c("state", "indicator", "gender"), ~str_remove_all(., "> ")) %>%
  mutate_if(is.character, trimws) %>%
  select(date, year, month, indicator, gender, age, state, series_type, value)

underutilisation <- bind_rows(underutilisation_age, underutilisation_state, underutilisation_volume) %>%
  distinct() %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  mutate(`Underutilised total` = `Underemployed total` + `Unemployed total`) %>%
  pivot_longer(cols = c(8:length(.)), names_to = 'indicator') %>%
  filter(!is.na(value))

write_csv(underutilisation, "data-raw/underutilisation.csv")

usethis::use_data(underutilisation, overwrite = TRUE, compress = "xz")
