plot_na <- function(indicators,
                    industry_division = NULL,
                    industry_subdivision = NULL,
                    series_types = "Trend") {

    check_data <- national_accounts %>%
        dplyr::filter(indicator == indicators,
                      series_type == series_types)

    if (length(unique(check_data$industry)) == 1) {
        plot_type <- "bar"
    } else if (grepl("growth", indicators)) {
        plot_type <- "ts"
    }


    plot_title <- toupper("PLOT TITLE")
    plot_subtitle <- toupper("PLOT SUBTITLE")
    plot_caption <- toupper("PLOT CAPTION")

    if (plot_type == "bar") {
        plot_data <- plot_data %>%
            dplyr::filter()
    }

    plot_data <- industry_value_add %>%
        dplyr::filter(indicator == indicator,
                      industry == industry_division,
                      subdivision == industry_subdivision,
                      series_type == series_type)

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value)) + ggplot2::geom_line() + ggplot2::labs(x = NULL, y = NULL, title = plot_title,
        subtitle = plot_subtitle, caption = plot_caption)
}
