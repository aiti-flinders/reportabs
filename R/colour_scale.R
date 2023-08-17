#' AITI Colour Scale
#'
#' @param palette Name of colour palette
#' @param discrete TRUE for a discrete scale
#' @param reverse TRUE to reverse the order of the colours
#'
#' @return ggplot2 colour scale
#' @export
#'

scale_colour_aiti <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  pal <- aiti_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("aiti_", palette), palette = pal)
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
#' @return ggplot2 fill scale
#' @export
#'

scale_fill_aiti <- function(palette = "main", discrete = TRUE, reverse = FALSE) {

  pal <- aiti_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("aiti_", palette), palette = pal)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256))
  }
}
