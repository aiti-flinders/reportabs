## code to prepare `salm` dataset goes here
library(tidyverse)
library(sf)
download.file("https://docs.employment.gov.au/system/files/doc/other/salm_smoothed_sa2_datafiles_asgs_2016_-_december_quarter_2019.csv",
              destfile = "data-raw/salm_sa2.csv",
              mode = "wb")

raw <- read_csv("data-raw/salm_sa2.csv", skip = 1)

all_sa2 <- absmapsdata::sa22016 %>%
  as_tibble() %>%
  select(sa2_name_2016, sa2_main_2016)

salm <- raw %>%
  janitor::clean_names() %>%
  pivot_longer(cols = c(4:length(.)),
               names_to = "date",
               values_to = "value") %>%
  rename(indicator = data_item,
         sa2_name_2016 = statistical_area_level_2_sa2_2016_asgs,
         sa2_main_2016 = sa2_code_2016_asgs) %>%
  mutate(value = as.numeric(value),
         date = as.Date(paste0(date, "_01"), format = "%b_%y_%d"),
         sa2_main_2016 = as.character(sa2_main_2016)) %>%
  right_join(all_sa2) %>%
  complete(indicator, nesting(sa2_name_2016, sa2_main_2016), date) %>%
  filter(!is.na(date),
         !is.na(indicator)) %>%
  left_join(absmapsdata::sa22016) %>%
  select(indicator, date, value,  sa2_name_2016, geometry)

usethis::use_data(salm, overwrite = TRUE, compress= "xz")
