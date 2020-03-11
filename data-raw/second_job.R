#create a second job dataset based on the experimental data in 6150.0.55.003 Table 1.xls

library(tidyverse)
library(readabs)

abs_link <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&6150055003do001_2019202012.xls&6150.0.55.003&Data%20Cubes&36497972FF4BC954CA2585270013DF3C&0&December%202019&11.03.2020&Latest"

if(!file.exists("data-raw/6150.0.55.003 Table 1.xls")) {

  download.file(abs_link,
    destfile = "data-raw/6150.0.55.003 Table 1.xls",
    mode = 'wb')
}

raw <- read_abs_local(path = "data-raw", filenames = "6150.0.55.003 Table 1.xls")

second_job <- raw %>%
  separate(series,
    into = c("prefix", "indicator", "state", "industry"),
    sep = ";",
    extra = "drop") %>%
  mutate_if(is.character, trimws) %>%
  filter(indicator == "Labour Account secondary jobs" | indicator == "Labour Account main jobs",
    series_type == "Trend") %>%
  mutate(value = 1000*value,
    year = lubridate::year(date),
    month = lubridate::month(date, abbr = FALSE, label = TRUE)) %>%
  select(date, month, year, prefix, indicator, state, industry, series_type, value, unit)

write_csv(second_job, 'data-raw/second_job.csv')

usethis::use_data(second_job, overwrite = TRUE, compress = 'xz')

