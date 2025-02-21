#' Generate AITI colours from a palette
#'
#' @param palette Name of colour palette
#' @param reverse TRUE to reverse the order of the colours
#' @param ... Other arguments passed to `colorRampPalette()`
#'
#' @export
#' @description
#' `r lifecycle::badge("deprecated")`
#' `aiti_pal()` has been renamed `fof_pal()`
#'
aiti_pal <- function(palette = "main", reverse = FALSE, ...) {

  lifecycle::deprecate_warn("0.0.2", "aiti_pal()", "fof_pal()")

  fof_pal(palette, reverse, ...)
}

#' Generate Factory of the Future colours from a pre-defined palette
#'
#' @param palette Name of colour palette. See `palette_names()` for a list of available palettes.
#' @param reverse Whether the order of the colours should be reversed (default FALSE)
#' @param ... Other arguments passed to `colorRampPalette()`
#'
#' @returns A colour palette
#' @export
#'
#' @examples
#' # To generate a sequence of n colours from the 'main' palette:
#' n <- 2
#' fof_pal("main")(n)
fof_pal <- function(palette = "main", reverse = FALSE, ...) {

  if (!palette %in% palette_names()) {
    cli::cli_abort(c("There is no palette called {.var palette}.",
                     "See `palette_names()` for a list of available palettes."))
  }

  pal <- fof_palettes[[palette]]

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
  names(fof_palettes)
}

#' List available colours
#'
#' @param colour A character of the name of the colour. If NULL, returns all available colours.
#'
#' @return A named character vector of the defined colours in the package.
#' @export
#'
#' @examples aiti_cols()
#' @description
#' `r lifecycle::badge("deprecated")`
#' `aiti_cols()` has been renamed `fof_cols()`
#'
aiti_cols <- function(colour = NULL) {

  lifecycle::deprecate_warn("0.0.2", "aiti_cols()", "fof_cols()")
  fof_cols(colour)
}

#' List available colours
#'
#' @param colour A character of the name of the colour. If NULL, returns all available colours.
#'
#' @returns A named character vector of the defined colours in the package.
#' @export
#'
#' @examples fof_cols()
fof_cols <- function(colour = NULL) {
  cols <- c(colour)

  if (is.null(cols))
    return (fof_colours)

  fof_colours[cols]
}



#' Visualise individual palettes
#'
#' @param palette Name of colour palette
#' @param n Number of colours required
#'
#' @returns NULL
#' @export
#'
#' @examples visualise_palette("just orange", n = 5)
visualise_palette <- function(palette, n) {

  cols <- fof_pal(palette)(n)

  scales::show_col(cols)
}


