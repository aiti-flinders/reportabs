## code to prepare `internet_vacancies_skill` dataset goes here
library(tidyverse)
library(readxl)

force <- TRUE

if (!file.exists("data-raw/internet_vacancies_skill.xlsx") | force) {

  download.file("http://lmip.gov.au/PortalFile.axd?FieldID=2790179&.xlsx",
    destfile = "data-raw/internet_vacancies_skill.xlsx",
    mode = "wb")
}

raw <- read_excel("data-raw/internet_vacancies_skill.xlsx",
  sheet = "Trend",
  .name_repair = "universal")

internet_vacancies_skill <- raw %>%
  pivot_longer(c(5:length(.)), names_to = 'date') %>%
  mutate(date = gsub("[...]", "", date),
    date = as.Date(as.numeric(date), origin = '1899-12-30'),
    state = strayr::strayr(State, to = "state_name", fuzzy_match = FALSE),
    state = ifelse(is.na(state), "Australia", state)) %>%
  group_by(state, date, skill = Skill_level) %>%
  summarise(value = mean(value)) %>%
  filter(skill != 0) %>%
  group_by(state, date) %>%
  mutate(share = value/sum(value)) %>%
  ungroup()

write_csv(internet_vacancies_skill, 'data-raw/internet_vacancies_skill')

usethis::use_data(internet_vacancies_skill, overwrite = TRUE, compress = "xz")

