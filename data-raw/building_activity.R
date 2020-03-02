## code to prepare `building_activity` dataset goes here
library(tidyverse)
library(readabs)

force <- TRUE

if(!file.exists("data-raw/building_activity.csv") | force) {

  read_abs(cat_no = "8752.0", table = 1, retain_files = FALSE) %>%
    write_csv("data-raw/building_activity.csv")

}

raw <- read_csv("data-raw/building_activity.csv")

building_activity <- raw %>%
  separate_series(column_names = c("indicator", "gender", "state")) %>%
  mutate(
    value = ifelse(unit == "000", (1000*value), (value)),
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    age = "Total (age)"
  )

write_csv(labour_force, "data-raw/labour_force.csv")
usethis::use_data("building_activity")
