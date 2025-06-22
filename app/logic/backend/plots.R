box::use(
    gg = ggplot2,
    sh = shiny,
    gir = ggiraph,
    dp = dplyr[`%>%`],
    lub = lubridate,
    str = stringr,
)

box::use(
    fe = app / logic / frontend,
    app / logic / frontend / themes,
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

# Degree to which grey border is less opaque than actual shapes
# This makes the real color stand out more since those are also opaque to some degree
ALPHADIFF <- 0.3

transcribe <- function(x) {
    ux <- unique(x)
    if (any(ux %in% c("z_strivings", "z_concerns"))) {
        # At app startup, there is a brief period where HOF subscales are both active (this is probably the real bug!)
        # To prevent a `textbox_grob() is not vectorised` error, the selected HOF subscales are also collapsed into a single string
        # even though there _should_ never be a scenario where two HOF subscales are active simultaneously
        paste0(str$str_to_title(str$str_extract(ux, "[^z_].+")), collapse = ", ")
    } else {
        toupper(paste0(ux, collapse = ", "))
    }
}

#' @export
create_label <- function(data, .name = "lab") {
    dp$mutate(
        data,
        !!.name := paste0(
            authors, "\n",
            subscale, ": ", plotvalue, "\n",
            "N: ", n_sample, "\n"
        )
    )
}

#' @export
plot_interactive <- function(data, colors = themes$plot_palette, background = "#ffffff", shapes = 21:23, alpha = 0.8) {
    # Stop if reactive
    stopifnot(!sh$is.reactive(data))

    min_x <- if (nrow(data) > 0) min(data$year_adj, na.rm = TRUE)
    max_x <- if (nrow(data) > 0) max(data$year_adj, na.rm = TRUE)

    data %>%
        create_label() %>%
        gg$ggplot(gg$aes(year_as_date, plotvalue)) +
        gir$geom_point_interactive(
          gg$aes(
            fill = country,
            shape = country,
            size = n_sample,
            tooltip = lab,
            data_id = id
          ),
          alpha = alpha,
          color = "black"
        ) +
        gg$scale_fill_manual(values = colors) +
        gg$scale_shape_manual(values = shapes) +
        gg$labs(
            x = "Year",
            color = "Country",
            y = "Perfectionism Mean",
            title = paste0("Perfectionism Observatory: ", min_x, " - ", max_x),
            subtitle = paste0(
                if (length(unique(data$subscale)) > 1) "Subscales: " else "Subscale: ",
                transcribe(data$subscale)
            )
        ) +
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
plot_static <- function(data, colors = themes$plot_palette, shapes = 21:23, alpha = 0.6) {
    # Stop if reactive
    stopifnot(!sh$is.reactive(data))

    min_x <- if (nrow(data) > 0) min(data$year_adj, na.rm = TRUE)
    max_x <- if (nrow(data) > 0) max(data$year_adj, na.rm = TRUE)
    min_y <- if (nrow(data) > 0) min(data$plotvalue, na.rm = TRUE) else -3
    max_y <- if (nrow(data) > 0) max(data$plotvalue, na.rm = TRUE) else -3

    data %>%
        gg$ggplot(gg$aes(year_as_date, plotvalue)) +
        gg$geom_point(gg$aes(fill = country, shape = country, size = n_sample), color = "black", alpha = alpha) +
        gg$scale_fill_manual(values = colors) +
        gg$scale_shape_manual(values = shapes) +
        gg$labs(
            x = "Year",
            shape = "Country",
            fill = "Country",
            y = "Perfectionism Mean",
            title = paste0("Perfectionism Observatory: ", min_x, " - ", max_x),
            subtitle = paste0(
                if (length(unique(data$subscale)) > 1) "Subscales: " else "Subscale: ",
                transcribe(data$subscale)
            ),
            caption = paste0("Accessed ", lub$today(), "\n @ <link-to-page>")
        ) +
        #gg$ylim(c(max(min_y - 0.5, 0), max_y + 0.5)) +
        gg$scale_size(guide = "none") + # No legend for size aes
        gg$theme_bw() +
        fe$ggtheme
}
