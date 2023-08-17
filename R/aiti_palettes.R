#' Generate AITI colours from a palette
#'
#' @param palette Name of colour palette
#' @param reverse TRUE to reverse the order of the colours
#' @param ... Other arguments passed to `colorRampPalette()`
#'
#' @importFrom grDevices colorRampPalette
#'
#'
aiti_pal <- function(palette = "main", reverse = FALSE, ...) {

  if (!palette %in% palette_names()) {
    cli::cli_abort(c("There is no palette called {.var palette}.",
                     "See `palette_names()` for a list of available palettes."))
  }

  pal <- aiti_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}

#' List available palettes
#'
#' @return character vector of palette names
#' @export
#'
#' @examples palette_names()
palette_names <- function() {
  names(aiti_palettes)
}

#' List available colours
#'
#' @param colour A character of the name of the colour. If NULL, returns all available colours.
#'
#' @return A named character vector of the defined colours in the package.
#' @export
#'
#' @examples aiti_cols()
aiti_cols <- function(colour = NULL) {
  cols <- c(colour)

  if (is.null(cols))
    return (aiti_colours)

  aiti_colours[cols]
}



#' Visualise individual palettes
#'
#' @param palette Name of colour palette
#' @param n Number of colours required
#'
#' @return NULL
#' @export
#'
#' @examples visualise_palette("just orange", n = 5)
visualise_palette <- function(palette, n) {

  cols <- aiti_pal(palette)(n)

  scales::show_col(cols)
}


