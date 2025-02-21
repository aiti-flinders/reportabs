#' AITI Colour Scale
#'
#' @param palette Name of colour palette
#' @param discrete TRUE for a discrete scale
#' @param reverse TRUE to reverse the order of the colours
#'
#' @return ggplot2 colour scale
#' @export
#' @description
#' `r lifecycle::badge("deprecated")`
#' `scale_colour_aiti()` has been renamed `scale_colour_fof()`

scale_colour_aiti <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  lifecycle::deprecate_warn("0.0.2", "scale_colour_aiti()", "scale_colour_fof()")

  scale_colour_fof(palette,
                   discrete,
                   reverse)
}

#' Colour scale for Factory of the Future colours
#'
#' @param palette Name of the colour palette (default "main")
#' @param discrete Whether the colours should be discrete or continuous (default TRUE)
#' @param reverse Whether the order of the colours should be reversed (default FALSE)
#'
#' @returns ggplot2 colour scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = disp, col = factor(cyl))) + geom_point()
#' p + scale_colour_fof()
#'
scale_colour_fof <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  pal <- fof_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", name = paste0("fof", palette), palette = pal)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256))
  }

}

#' AITI Fill Scale
#'
#' @param palette Name of colour palette
#' @param discrete TRUE for a discrete scale
#' @param reverse TRUE to reverse the order of the colours
#'
#' @returns ggplot2 fill scale
#' @export
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `scale_fill_aiti()` has been renamed `scale_fill_fof()`

scale_fill_aiti <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  lifecycle::deprecate_warn("0.0.2", "scale_colour_aiti()", "scale_colour_fof()")

  scale_fill_fof(palette,
                   discrete,
                   reverse)
}

#' Fill scale for Factory of the Future colours
#'
#' @param palette Name of the colour palette (default "main")
#' @param discrete Whether the colours should be discrete or continuous (default TRUE)
#' @param reverse Whether the order of the colours should be reversed (default FALSE)
#'
#' @returns ggplot2 fill scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = c("One", "Two", "Three"), y = c(4, 2, 9))
#' p <- ggplot(df, aes(x = x, y = y, fill = x)) + geom_col()
#' p + scale_fill_fof()
scale_fill_fof <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  pal <- fof_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", name = paste0("fof", palette), palette = pal)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256))
  }
}
