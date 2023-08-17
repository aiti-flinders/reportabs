devtools::load_all()

aiti_colours <- c(
  `AITI Orange` = "#E75730",
  `Neon Purple` = "#8000FF",
  `Flinders Gold` = "#FFd300",
  `Sustainable Green` = "#4AEAB2",
  `Soft Black` = "#2B2228",
  `Laurel Green` = "#4D5C5A",
  `Concrete Grey` = "#99A2A5",
  `Just White` = "#FFFFFF",
  `Hard Black` = "#000000",
  `Legacy Blue` = "#0B396D",
  `Legacy Blue Highlight` = "#0077FF"
)

usethis::use_data(aiti_colours, overwrite = TRUE)

aiti_palettes <- list(
  `main` = aiti_cols(c("AITI Orange", "Soft Black", "Laurel Green", "Concrete Grey")),
  `purple` = aiti_cols(c("Neon Purple", "Soft Black", "Laurel Green", "Concrete Grey")),
  `flinders` = aiti_cols(c("Flinders Gold",  "Laurel Green", "Concrete Grey")),
  `green` = aiti_cols(c("Sustainable Green", "Soft Black", "Laurel Green", "Concrete Grey")),
  `passive` = aiti_cols(c("Soft Black", "Laurel Green", "Concrete Grey")),
  `just orange` = aiti_cols(c("AITI Orange", "Just White")),
  `just green` = aiti_cols(c("Sustainable Green", "Hard Black")),
  `legacy` = aiti_cols(c("Legacy Blue", "Legacy Blue Highlight"))
)

usethis::use_data(aiti_palettes, overwrite = TRUE)
