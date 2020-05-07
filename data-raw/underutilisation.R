## code to prepare `underutilisation` dataset goes here
library(readabs)
library(tidyverse)

force_update <- TRUE

any_file_missing <- !file.exists("data-raw/underutilisation_age.csv") |
  !file.exists("data-raw/underutilisation_state.csv") |
  !file.exists("data-raw/underutilisation_volume.csv")



if(any_file_missing | force_update) {

  read_abs(cat_no = "6291.0.55.003", tables = c('23a', '23b'), retain_files = FALSE) %>%
    write_csv("data-raw/underutilisation.csv")


}

raw <- read_csv("data-raw/underutilisation.csv")

underutilisation_23a <- raw %>%
  filter(table_no == "6291023a") %>%
  separate(series, into = c("state", "indicator", "gender"), sep = ";") %>%
  mutate_at(c("state", "indicator", "gender"), ~trimws(str_remove_all(., ">"))) %>%
  mutate(age = "Total (age)",
         value = ifelse(unit == "000", (1000*value), value),
         year = lubridate::year(date),
         month = lubridate::month(date, label = T, abbr = F)) %>%
  select(date, year, month, indicator, gender, age, state, series_type, value, unit)

underutilisation_23b <- raw %>%
  filter(table_no == "6291023b") %>%
  separate(series, into = c("age", "indicator", "gender"), sep = ";", fill = 'left') %>%
  mutate_at(c("age", "indicator", "gender"), ~trimws(str_remove_all(., ">"))) %>%
  mutate(gender = ifelse(gender == "", indicator, gender),
         indicator = ifelse(indicator %in% c("Persons", "Males", "Females"), age, indicator),
         age = ifelse(age == indicator, "Total (age)", age),
         state = "Australia",
         year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  select(date, year, month, indicator, gender, age, state, series_type, value, unit)


underutilisation <- bind_rows(underutilisation_23a, underutilisation_23b) %>%
  distinct()

write_csv(underutilisation, "data-raw/underutilisation.csv")

usethis::use_data(underutilisation, overwrite = TRUE, compress = "xz")
