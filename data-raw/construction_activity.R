## code to prepare `construction_activity` dataset goes here
library(tidyverse)
library(readabs)

if(!file.exists("data-raw/construction_activity.csv")) {

  read_abs(cat_no = "8762.0", table = 1, retain_files = FALSE) %>%
    write_csv("data-raw/construction_activity.csv")

}

raw <- readr::read_csv("data-raw/construction_activity.csv")

construction_activity <- raw %>%
  separate_series(column_names = c("indicator", "measure", "sector")) %>%
  mutate(
    value = ifelse(unit == "000", (1000*value), (value)),
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE)
    )

write_csv(construction_activity, "data-raw/construction_activity.csv")

usethis::use_data(construction_activity, overwrite = TRUE, compress = 'xz')
