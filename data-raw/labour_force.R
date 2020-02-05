## code to prepare `labour_force` dataset goes here
library(readabs)
library(tidyverse)

if(!file.exists("data-raw/labour_force.csv")) {

  read_abs(cat_no = "6202.0", table = 12, retain_files = FALSE) %>%
    write_csv("data-raw/labour_force.csv")

}

raw <- readr::read_csv("data-raw/labour_force.csv")

labour_force <- raw %>%
  separate_series(column_names = c("indicator", "gender", "state")) %>%
  mutate(
    value = ifelse(unit == "000", (1000*value), (value)),
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    age = "Total (age)"
    )

write_csv(labour_force, "data-raw/labour_force.csv")

usethis::use_data(labour_force, overwrite = TRUE, compress = 'xz')
