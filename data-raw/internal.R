# code to prepare `fof_colours` dataset goes here
fof_colours <- c(
  `Factory Orange` = "#E75730",
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



fof_palettes <- list(
  `main` = fof_colours[c("Factory Orange", "Soft Black", "Laurel Green", "Concrete Grey")],
  `purple` = fof_colours[c("Neon Purple", "Soft Black", "Laurel Green", "Concrete Grey")],
  `flinders` = fof_colours[c("Flinders Gold",  "Laurel Green", "Concrete Grey")],
  `green` = fof_colours[c("Sustainable Green", "Soft Black", "Laurel Green", "Concrete Grey")],
  `passive` = fof_colours[c("Soft Black", "Laurel Green", "Concrete Grey")],
  `just orange` = fof_colours[c("Factory Orange", "Just White")],
  `just green` = fof_colours[c("Sustainable Green", "Laurel Green")],
  `legacy` = fof_colours[c("Legacy Blue", "Legacy Blue Highlight")]
)

## code to prepare `labour_force_briefing` dataset goes here





usethis::use_data(fof_palettes, fof_colours, internal = TRUE, overwrite = TRUE)


