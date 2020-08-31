plot_na <- function(indicator, industry_division, industry_subdivision, series_type) {
    
    if (indicator == "GVA" & is.null(industry_division) & is.null(industry_subdivision)) {
        stop("For indicator == 'GVA' must specify at least one of industry_division or industry_division")
    }
    
    if (indicator == "GVA" & !is.null(industry_division) & is.null(industry_subdivision)) {
        industry_subdivision <- paste(industry_division, "(Total)")
    }
    
    plot_title <- toupper()
    
    plot_data <- daitir::industry_value_add %>% dplyr::filter(indicator == indicator, industry == industry_division, subdivision == industry_subdivision, 
        series_type == series_type)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value)) + ggplot2::geom_line() + ggplot2::labs(x = NULL, y = NULL, title = plot_title, 
        subtitle = plot_subtitle, caption = plot_caption)
}
