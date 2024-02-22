box::use(
    sh = shiny,
    e4r = echarts4r,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr[`%>%`],
    htw = htmlwidgets,
    gg = ggplot2,
    lub = lubridate,
)

box::use(
    fe = app / logic / frontend,
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
main_ui <- function(id, nav_title, card_title = NULL) {
    ns <- sh$NS(id)
    bsl$nav_panel(
        nav_title,
        bsl$card_title(card_title),
        bsl$card_body(e4r$echarts4rOutput(ns("plot")))
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        min_x <- sh$reactive(min(data()$year))
        max_x <- sh$reactive(max(data()$year))

        res_download <- sh$reactive({
            data() %>%
                dp$mutate(scale = toupper(scale)) %>%
                dp$rename(Scale = scale, `Sample size` = N) %>%
                gg$ggplot(gg$aes(year, value, group = Scale, fill = Scale)) +
                gg$geom_point(gg$aes(size = `Sample size`), shape = 21, color = "black", alpha = 0.7) +
                gg$scale_fill_manual(values = c("#aee0fa", "#92bc92", "#fefee1")) +
                gg$labs(
                    x = "Year",
                    y = "Value",
                    title = paste0("Perfectionism Observatory: ", min_x(), " - ", max_x()),
                    subtitle = paste0(
                        if (length(unique(data()$scale)) > 1) "Scales: " else "Scale: ",
                        toupper(paste0(unique(data()$scale), collapse = ", ")),
                        " for subscale: ",
                        toupper(unique(data()$subscale))
                    ),
                    caption = paste0("Accessed ", lub$today(), "\n @ <link-to-page>")
                ) +
                gg$scale_size(guide = "none") + # No legend for size aes
                gg$theme_bw() +
                fe$ggtheme
        })

        res_interactive <- sh$reactive({
            data() %>%
                # Turn year into factor to avoid decimals on small year input ranges
                dp$mutate(year = factor(year)) %>%
                dp$group_by(scale) %>%
                dp$arrange(year) %>%
                # Begin echart
                e4r$e_charts(year) %>%
                e4r$e_scatter(value, N, bind = author) %>%
                e4r$e_tooltip(
                    formatter = htw$JS("
                        function(params){
                            return('<strong>' + params.name +
                                    '</strong><br />Year: ' + params.value[0] +
                                    '<br />Value: ' + params.value[1] +
                                    '<br />N: ' + params.value[2]
                                )
                        }
                    "),
                    trigger = "item"
                ) %>%
                e4r$e_legend(bottom = 0) %>%
                e4r$e_title(
                    text = paste0("Perfectionism Observatory: ", min_x(), " - ", max_x()),
                    subtext = paste0(
                        if (length(unique(data()$scale)) > 1) "Scales: " else "Scale: ",
                        toupper(paste0(unique(data()$scale), collapse = ", ")),
                        " for subscale: ",
                        toupper(unique(data()$subscale))
                    ),
                ) %>%
                e4r$e_theme_custom("app/static/echarts_theme.json")
        })

        output$plot <- e4r$renderEcharts4r(res_interactive())

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
