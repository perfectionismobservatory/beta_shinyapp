box::use(
    gg = ggplot2,
    sh = shiny,
    gir = ggiraph,
    dp = dplyr[`%>%`],
)

box::use(
    fe = app / logic / frontend,
)

css_default_hover <- gir$girafe_css_bicolor(primary = "yellow", secondary = "red")

gir$set_girafe_defaults(
    opts_hover = gir$opts_hover(css = css_default_hover),
    opts_tooltip = gir$opts_tooltip(css = "padding:10px;background-color:white;color:black;border-radius:5px;border:solid 4px #7A9DAF;")
)

# Size multiplier for dark points behind color points
# This creates bordered shapes independently of those shapes that come with borders
# The problem with these bordered shapes is how they show in the legend
SIZEMUL <- 1.2

#' @export
create_label <- function(data, .name = "lab") {
    dp$mutate(
        data,
        !!.name := paste0(
            authors, "\n",
            subscale, ": ", mean_adj, "\n",
            "N: ", n_sample, "\n"
        )
    )
}

#' @export
plot_interactive <- function(data, background = "#ffffff") {
    # Stop if reactive
    stopifnot(!sh$is.reactive(data))

    min_x <- if (nrow(data) > 0) min(data$year, na.rm = TRUE)
    max_x <- if (nrow(data) > 0) max(data$year, na.rm = TRUE)

    data %>%
        create_label() %>%
        gg$ggplot(gg$aes(year_as_date, mean_adj, shape = country)) +
        gir$geom_point_interactive(gg$aes(size = inv_var * SIZEMUL), color = "grey20", alpha = 0.6, show.legend = TRUE) +
        gir$geom_point_interactive(gg$aes(color = subscale, size = inv_var, tooltip = lab, data_id = id), alpha = 0.6) +
        gg$scale_color_manual(values = c("#aee0fa", "#92bc92", "#fefee1", "#57707d", "#495e49", "#7f7f71")) +
        gg$labs(
            x = "Year",
            color = "Subscale",
            shape = "Country",
            y = "Value",
            title = paste0("Perfectionism Observatory: ", min_x, " - ", max_x),
            subtitle = paste0(
                if (length(unique(data$subscale)) > 1) "Subscales: " else "Subscale: ",
                toupper(paste0(unique(data$subscale), collapse = ", "))
            )
        ) +
        gg$ylim(0, NA) +
        gg$scale_size(guide = "none") + # No legend for size aes
        gg$theme_bw() +
        fe$ggtheme +
        gg$theme(
            panel.background = gg$element_rect(fill = background),
            plot.background = gg$element_rect(fill = background, color = background),
            legend.background = gg$element_rect(fill = background, color = background)
        )
}

#' @export
plot_static <- function(data) {
    # Stop if reactive
    stopifnot(!sh$is.reactive(data))

    min_x <- if (nrow(data) > 0) min(data$year, na.rm = TRUE)
    max_x <- if (nrow(data) > 0) max(data$year, na.rm = TRUE)

    data %>%
        gg$ggplot(gg$aes(year_as_date, mean_adj)) +
        gg$geom_point(gg$aes(size = inv_var * SIZEMUL), color = "grey20", alpha = 0.6, show.legend = TRUE) +
        gg$geom_point(gg$aes(color = subscale, size = inv_var, tooltip = lab, data_id = id), alpha = 0.6) +
        gg$scale_color_manual(values = c("#aee0fa", "#92bc92", "#fefee1", "#57707d", "#495e49", "#7f7f71")) +
        gg$labs(
            x = "Year",
            color = "Subscale",
            shape = "Country",
            y = "Value",
            title = paste0("Perfectionism Observatory: ", min_x, " - ", max_x),
            subtitle = paste0(
                if (length(unique(data$subscale)) > 1) "Subscales: " else "Subscale: ",
                toupper(paste0(unique(data$subscale), collapse = ", "))
            ),
            caption = paste0("Accessed ", lub$today(), "\n @ <link-to-page>")
        ) +
        gg$ylim(0, NA) +
        gg$scale_size(guide = "none") + # No legend for size aes
        gg$theme_bw() +
        fe$ggtheme
}
