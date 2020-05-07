## code to prepare `internet_vacancies` dataset goes here
library(tidyverse)
library(readxl)

force <- TRUE

if (!file.exists("data-raw/internet_vacancies_basic.xlsx") | force) {

  download.file("http://lmip.gov.au/PortalFile.axd?FieldID=2790177&.xlsx",
    destfile = "data-raw/internet_vacancies_basic.xlsx",
    mode = "wb")
  }

raw <- read_excel("data-raw/internet_vacancies_basic.xlsx",
                  sheet = 1,
                  .name_repair = "universal")

internet_vacancies_basic <- raw %>%
  gather(key = "date", value = "vacancies", c(5:length(.))) %>%
  mutate(date = as.numeric(gsub("[...]","", date)),
         date = as.Date(date, origin = "1899-12-30"),
         state = strayr::strayr(State, to = "state_name", fuzzy_match = FALSE),
         state = ifelse(is.na(state), "Australia", state)) %>%
  group_by(state, date, ANZSCO_CODE, Title) %>%
  summarise(vacancies=mean(vacancies)) %>%
  rename(region = state,
         occupation = Title,
         anzsco_2 = ANZSCO_CODE) %>%
  ungroup()

write_csv(internet_vacancies_basic, 'data-raw/internet_vacancies_basic.csv')

usethis::use_data(internet_vacancies_basic, overwrite = TRUE, compress = "xz")
#
#
# read_vacancy_report <- function(type = "all") {
#   if(type == 'skill') {
#     read_excel('data/lmip_vacancy_skill.xlsx', sheet = "Trend", .name_repair = 'universal') %>%
#       gather(key = 'date', value = 'vacancies', c(5:length(.))) %>%
#       mutate(date = gsub("[...]", "", date),
#              date = as.Date(as.numeric(date), origin = '1899-12-30'),
#              region = strayr(State, to = 'state_name', fuzzy_match = F),
#              region = ifelse(is.na(region), "Australia", region)) %>%
#       group_by(region, date, Skill_level) %>%
#       summarise(vacancies = mean(vacancies))%>%
#       filter(Skill_level != 0) %>%
#       group_by(region, date) %>%
#       mutate(share = vacancies/sum(vacancies)) %>%
#       ungroup()
#
#   } else if(type == 'occ') {
#     read_excel('data/lmip_vacancy_occupation.xlsx', sheet = 2, .name_repair = 'universal') %>%
#       gather(key = 'date', value = 'vacancies', c(4:length(.))) %>%
#       mutate(date = gsub("[...]", "", date),
#              date = as.Date(as.numeric(date), origin = '1899-12-30'),
#              state = strayr(state, to = 'state_name', fuzzy_match = F),
#              state = ifelse(is.na(state), "Australia", state),
#              vacancies = as.numeric(vacancies)) %>%
#       group_by(state, date, ANZSCO_TITLE, ANZSCO_CODE) %>%
#       summarise(vacancies = mean(vacancies, na.rm=T)) %>%
#       rename(occ = ANZSCO_TITLE,
#              anzsco_4 = ANZSCO_CODE,
#              region = state) %>%
#       mutate(anzsco_1 = str_sub(anzsco_4, start = 1, end = 1),
#              anzsco_2 = str_sub(anzsco_4, start = 1, end = 2),
#              anzsco_3 = str_sub(anzsco_4, start = 1, end = 3)) %>%
#       mutate_at(.vars = c('anzsco_1', 'anzsco_2', 'anzsco_3'),
#                 .funs = function(x) str_pad(x, width = 4, side = 'right',pad = '0')) %>%
#       ungroup()
#   } else if(type == 'basic') {
#     read_excel('data/lmip_vacancy.xlsx', sheet = 1, .name_repair = 'universal') %>%
#       gather(key = 'date', value = "vacancies", c(5:length(.))) %>%
#       mutate(date = gsub("[...]","", date),
#              date = as.Date(as.numeric(date), origin = "1899-12-30"),
#              State = strayr(State, to = 'state_name', fuzzy_match = F),
#              State = ifelse(is.na(State), "Australia", State)) %>%
#       group_by(State, date, ANZSCO_CODE, Title) %>%
#       summarise(vacancies=mean(vacancies)) %>%
#       rename(region = State,
#              occ = Title,
#              anzsco_2 = ANZSCO_CODE) %>%
#       ungroup()
#   } else if (type == 'all') {
#     bind_rows(read_vacancy_report(type = 'basic'),
#               read_vacancy_report(type = 'skill'),
#               read_vacancy_report(type = 'occ'))
#   }
#
# }


