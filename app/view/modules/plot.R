box::use(
    sh = shiny,
    e4r = echarts4r,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr[`%>%`],
    htw = htmlwidgets,
)

box::use(
    fe = app / logic / frontend,
)

#' @export
header_ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        class = "d-flex flex-row gap-2 align-items-center",
        sh$actionButton(
            class = "btn btn-secondary hover bg-transparent border-0 p-2",
            ns("download-default"),
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
        )
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

        res_interactive <- sh$reactive({
            min_x <- min(data()$year)
            max_x <- max(data()$year)
            data() %>%
                dp$group_by(scale) %>%
                e4r$e_charts(year) %>%
                e4r$e_scatter(value, N, bind = author) %>%
                # This would color the points after N as well
                # e4r$e_visual_map_("N", scale = e4r$e_scale) %>%
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
                    # textStyle = list(fontFamily = "Comissioner"),
                    trigger = "item"
                ) %>%
                e4r$e_legend(bottom = 0) %>%
                e4r$e_title(
                    text = paste0(
                        "Viewing ",
                        if (length(unique(data()$scale)) > 1) "scales: " else "scale: ",
                        toupper(paste0(unique(data()$scale), collapse = ", ")),
                        " for subscale: ",
                        toupper(unique(data()$subscale))
                    ),
                    subtext = "We could also have a subtitle"
                ) %>%
                e4r$e_x_axis(min = min_x, max = max_x) %>%
                e4r$e_theme_custom("app/static/echarts_theme.json")
        })

        output$plot <- e4r$renderEcharts4r(res_interactive())
    })
}
