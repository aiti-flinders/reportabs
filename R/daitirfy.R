#internal function to convert old abs files to daitir format
daitirfy <- function(data) {

  states <- c(
    "New South Wales",
    "Victoria",
    "Queensland",
    "South Australia",
    "Western Australia",
    "Tasmania",
    "Northern Territory",
    "Australian Capital Territory"
    )

  labour_force_12 <- data %>%
    dplyr::filter(table_no == 6202012)  %>%
    readabs::separate_series(column_names = c("indicator", "gender", "state")) %>%
    dplyr::mutate(
      value = ifelse(unit == "000", (1000*value), (value)),
      year = lubridate::year(date),
      month = lubridate::month(date, label = TRUE, abbr = FALSE),
      age = "Total (age)"
    ) %>%
    dplyr::select(date, year, month, indicator,  gender, age, state, series_type, value, unit)


  labour_force_19 <- data %>%
    dplyr::filter(table_no == 6202019) %>%
    tidyr::separate(series, into = c("indicator", "gender", "state"), sep = ";") %>%
    dplyr::mutate(dplyr::across(c(indicator, gender), ~trimws(gsub(">", "", .))),
           state = ifelse(gender %in% states, gender, "Australia"),
           gender = ifelse(gender %in% states, "Persons", gender),
           unit = "000",
           value = ifelse(unit == "000", 1000*value, value),
           year = lubridate::year(date),
           month = lubridate::month(date, label = TRUE, abbr = FALSE),
           age = "Total (age)"
    ) %>%
    dplyr::select(date, year, month, indicator, gender, age, state, series_type, value, unit)


  labour_force_22 <- data %>%
    dplyr::filter(table_no == 6202022) %>%
    tidyr::separate(series, into = c("indicator", "gender", "age"), sep = ";") %>%
    dplyr::mutate(dplyr::across(c(indicator, gender, age), ~trimws(gsub(">", "", .))),
           age = ifelse(age == "", "Total (age)", age),
           value = ifelse(unit == "000", (1000*value), value),
           year = lubridate::year(date),
           month = lubridate::month(date, label = T, abbr = F),
           state = "Australia") %>%
    dplyr::select(date, year, month, indicator, gender, age, state, series_type, value, unit)


  labour_force_23 <- data %>%
    dplyr::filter(table_no == 6202023) %>%
    tidyr::separate(series, into = c("indicator", "gender", "state"), sep = ";") %>%
    dplyr::mutate(dplyr::across(c(indicator, gender, state), ~trimws(gsub(">", "", .))),
           state = ifelse(state == "", "Australia", state),
           value = ifelse(unit == "000", (1000*value), value),
           year = lubridate::year(date),
           month = lubridate::month(date, label = T, abbr = F),
           age = "Total (age)") %>%
    dplyr::select(date, year, month, indicator, gender, age, state, series_type, value, unit)

  labour_force <- dplyr::bind_rows(list(labour_force_12, labour_force_19, labour_force_22, labour_force_23)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
    dplyr::mutate("Underutilised total" = `Unemployed total` + `Underemployed total`) %>%
    tidyr::pivot_longer(cols = c(9:length(.)), names_to = "indicator", values_to = "value", values_drop_na = TRUE)

  return(labour_force)
}
