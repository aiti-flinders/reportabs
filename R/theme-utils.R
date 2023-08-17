points_to_mm <- function(points) {
  as.numeric(grid::convertX(ggplot2::unit(points, "points"), "mm"))[1]
}

#showtext setup for Roboto
enable_aiti_fonts <- function(dpi = 72) {
  sysfonts::font_add_google("Roboto", "Roboto")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = {{dpi}})

}

install_aiti_fonts <- function(theme = c("old", "new")) {
  rlang::check_installed("sysfonts")

  theme <- match.arg(theme)

  font <- switch(
    theme,
    old = "Roboto",
    new = "Space Mono"
  )

  cli::cli_progress_step("Installing {font} font from Google Fonts")
  sysfonts::font_add_google(font, font)
  cli::cli_progress_step("Successfully installed {font} font")

}
