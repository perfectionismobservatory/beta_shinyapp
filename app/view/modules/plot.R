box::use(
    sh = shiny,
    e4r = echarts4r,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr[`%>%`],
    htw = htmlwidgets,
    gg = ggplot2,
    lub = lubridate,
    scales,
    gir = ggiraph,
)

box::use(
    fe = app / logic / frontend,
)

# Increase point size by 20% to get borders around shapes
# The bordered shapes (# 21 etc) behave strangely with legends
# ... at least for me :)
SIZEMUL <- 1.2

css_default_hover <- gir$girafe_css_bicolor(primary = "yellow", secondary = "red")

gir$set_girafe_defaults(
    opts_hover = gir$opts_hover(css = css_default_hover),
    opts_tooltip = gir$opts_tooltip(css = "padding:10px;background-color:white;color:black;border-radius:5px;border:solid 4px #7A9DAF;")
)

#' @export
header_ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        class = "d-flex flex-row gap-2 align-items-center",
        sh$downloadButton(
            icon = NULL,
            class = "btn btn-secondary hover bg-transparent border-0 p-2",
            ns("download"),
            sh$div(
                class = "d-flex gap-2 align-items-center",
                bsi$bs_icon("download", size = "1.25rem"), "Download"
            )
        ),
        sh$actionButton(
            class = "btn btn-secondary hover bg-transparent border-0 p-2",
            ns("customise"),
            sh$div(
                class = "d-flex gap-2 align-items-center",
                bsi$bs_icon("brush", size = "1.25rem"), "Customise"
            )
        ) %>% bsl$tooltip("Feature in development")
    )
}

#' @export
sidebar_ui <- function(id) {
    ns <- sh$NS(id)
    bsl$accordion_panel(
        "Analysis",
        icon = bsi$bs_icon("graph-up-arrow"),
        fe$toggleswitch(ns("regression"), "Toggle regression line")
    )
}

#' @export
main_ui <- function(id, card_title = NULL) {
    ns <- sh$NS(id)
    bsl$nav_panel(
        bsl$card_title(card_title),
        bsl$card_body(gir$girafeOutput(ns("plot")))
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # FIX on first evaluation of reactive graph, dataframe is a 0-row df
        # Code below is a bandaid solution only
        min_x <- sh$reactive(if (nrow(data()) > 0) min(data()$year, na.rm = TRUE))
        max_x <- sh$reactive(if (nrow(data()) > 0) max(data()$year, na.rm = TRUE))

        res_download <- sh$reactive({
            data() %>%
                gg$ggplot(gg$aes(year_as_date, mean)) +
                gg$geom_point(gg$aes(size = inv_var * SIZEMUL, shape = country), color = "grey20", show.legend = TRUE) +
                gg$geom_point(gg$aes(color = subscale, size = inv_var, shape = country)) +
                gg$scale_color_manual(values = c("#aee0fa", "#92bc92", "#fefee1", "#57707d", "#495e49", "#7f7f71")) +
                gg$labs(
                    x = "Year",
                    color = "Subscale",
                    shape = "Country",
                    y = "Value",
                    title = paste0("Perfectionism Observatory: ", min_x(), " - ", max_x()),
                    subtitle = paste0(
                        if (length(unique(data()$subscale)) > 1) "Subscales: " else "Subscale: ",
                        toupper(paste0(unique(data()$subscale), collapse = ", "))
                    ),
                    caption = paste0("Accessed ", lub$today(), "\n @ <link-to-page>")
                ) +
                gg$ylim(0, NA) +
                gg$scale_size(guide = "none") + # No legend for size aes
                gg$theme_bw() +
                fe$ggtheme
        })

        res_interactive <- sh$reactive({
            data() %>%
                dp$mutate(lab = paste0(
                    authors, "\n",
                    subscale, ": ", mean, "\n",
                    "N: ", n_sample, "\n"
                )) %>%
                gg$ggplot(gg$aes(year_as_date, mean, shape = country)) +
                gir$geom_point_interactive(gg$aes(size = inv_var * SIZEMUL), color = "grey20", show.legend = TRUE) +
                gir$geom_point_interactive(gg$aes(color = subscale, size = inv_var, tooltip = lab, data_id = id), alpha = 0.9) +
                gg$scale_color_manual(values = c("#aee0fa", "#92bc92", "#fefee1", "#57707d", "#495e49", "#7f7f71")) +
                gg$labs(
                    x = "Year",
                    color = "Subscale",
                    shape = "Country",
                    y = "Value",
                    title = paste0("Perfectionism Observatory: ", min_x(), " - ", max_x()),
                    subtitle = paste0(
                        if (length(unique(data()$subscale)) > 1) "Subscales: " else "Subscale: ",
                        toupper(paste0(unique(data()$subscale), collapse = ", "))
                    )
                ) +
                gg$ylim(0, NA) +
                gg$scale_size(guide = "none") + # No legend for size aes
                gg$theme_bw() +
                fe$ggtheme +
                gg$theme(
                    panel.background = gg$element_rect(fill = "#f9fbfb"),
                    plot.background = gg$element_rect(fill = "#f9fbfb", color = "#f9fbfb"),
                    legend.background = gg$element_rect(fill = "#f9fbfb", color = "#f9fbfb")
                )
        })

        output$plot <- gir$renderGirafe(
            gir$girafe(
                ggobj = res_interactive(),
                width_svg = 7,
                height_svg = 4
            )
        )

        output$download <- sh$downloadHandler(
            filename = \() {
                paste(lub$today(), "perfectrepo.pdf", sep = "_")
            },
            content = \(file) {
                gg$ggsave(file, res_download(), width = 7, height = 5)
            }
        )
    })
}
