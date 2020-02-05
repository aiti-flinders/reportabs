## code to prepare `underemployment_by_industry` dataset goes here
library(readabs)
library(tidyverse)

force_update <- FALSE

if(!file.exists("data-raw/underemployment_by_industry_and_occupation.csv") | force_update) {

  read_abs(cat_no = "6291.0.55.003", tables = 19, retain_files = FALSE) %>%
    write_csv("data-raw/underemployment_by_industry_and_occupation.csv")

}

raw <- read_csv("data-raw/underemployment_by_industry_and_occupation.csv")

underemployment_by_industry_and_occupation <- raw %>%
  separate(series, into = c("industry", "indicator", "gender"), sep = ';',extra = 'drop') %>%
  mutate_at(c('industry', 'indicator', 'gender'), ~str_remove_all(., "> ")) %>%
  mutate_if(is.character, trimws) %>%
  mutate(year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    value = ifelse(unit == "000", value*1000, value),
    state = "Australia",
    age = "Total (age)",
    gender = ifelse(gender == "", indicator, gender),
    indicator = ifelse(indicator %in% gender, industry, indicator),
    industry = ifelse(industry %in% indicator, "Total all industries", industry)) %>%
  select(date, year, month, indicator, industry, gender, age, state, series_type, value)


usethis::use_data(underemployment_by_industry_and_occupation, overwrite = TRUE, compress = 'xz')
