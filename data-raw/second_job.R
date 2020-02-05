#create a second job dataset based on the experimental data in 6150.0.55.003 Table 1.xls

library(tidyverse)
library(readabs)

abs_link <- "https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&6150055003do001_2019202009.xls&6150.0.55.003&Data%20Cubes&44CAE02583753D0ECA2584CB0015635F&0&September%202019&10.12.2019&Latest"

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
  select(date, month, year, prefix, indicator, state, industry, value)

write_csv(second_job, 'data-raw/second_job.csv')

usethis::use_data(second_job, overwrite = TRUE, compress = 'xz')

