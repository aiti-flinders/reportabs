# code to prepare `fof_colours` dataset goes here
fof_colours <- c(
  `Summer Red` = "#Dc5300",
  `Flinders Gold` = "#FFd300",
  `Midnight Navy` = "#232D4b",
  `Sand` = "#F6EEE1",
  `Neon Purple` = "#8000FF",
  `Sustainable Green` = "#4AEAB2",
  `Soft Black` = "#2B2228",
  `Laurel Green` = "#4D5C5A",
  `Concrete Grey` = "#99A2A5",
  `Just White` = "#FFFFFF",
  `Hard Black` = "#000000",
  `Legacy Blue` = "#0B396D",
  `Legacy Blue Highlight` = "#0077FF"
)



fof_palettes <- list(
  `main` = fof_colours[c("Summer Red", "Soft Black", "Laurel Green", "Sand", "Midnight Navy")],
  `flinders` = fof_colours[c("Flinders Gold",  "Laurel Green", "Concrete Grey")],
  `blue` = fof_colours[c("Midnight Navy", "Soft Black", "Laurel Green", "Concrete Grey")],
  `passive` = fof_colours[c("Soft Black", "Laurel Green", "Concrete Grey", "Sand")],
  `just orange` = fof_colours[c("Summer Red", "Just White")],
  `just blue` = fof_colours[c("Midnight Navy", "Sand")],
  `legacy` = fof_colours[c("Legacy Blue", "Legacy Blue Highlight")]
)

usethis::use_data(fof_palettes, fof_colours, internal = TRUE, overwrite = TRUE)


