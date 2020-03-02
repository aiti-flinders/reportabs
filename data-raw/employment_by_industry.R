## code to prepare `underemployment_by_industry` dataset goes here
library(readabs)
library(tidyverse)

force_update <- TRUE

if(!file.exists("data-raw/underemployment_by_industry_and_occupation.csv") | force_update) {

  read_abs(cat_no = "6291.0.55.003", tables = 19, retain_files = FALSE) %>%
    write_csv("data-raw/underemployment_by_industry_and_occupation.csv")

  read_abs(cat_no = "6291.0.55.003", tables = 5, retain_files = FALSE) %>%
    write_csv("data-raw/employment_by_industry_and_state.csv")


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

raw <- read_csv("data-raw/employment_by_industry_and_state.csv")

employment_by_industry_and_state <- raw %>%
  separate(series, into = c("state", "industry", "indicator"), sep = ";", extra = "drop") %>%
  mutate_at(c("state", "industry", "indicator"), ~str_remove_all(., "> ")) %>%
  mutate_if(is.character, trimws) %>%
  mutate(indicator = ifelse(indicator == "", industry, indicator),
    industry = ifelse(str_detect(industry, "Employed"), "Total (industry)", industry),
    gender = "Persons",
    age = "Total (age)",
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    value = ifelse(unit == "000", value*1000, value)) %>%
  select(date, year, month, indicator, industry, gender, age, state, series_type, value)

employment_industry <- bind_rows(underemployment_by_industry_and_occupation, employment_by_industry_and_state) %>%
  distinct()


usethis::use_data(employment_industry, overwrite = TRUE, compress = 'xz')
