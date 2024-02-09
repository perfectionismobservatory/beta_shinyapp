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
ui_input <- function(id) {
    ns <- sh$NS(id)
    bsl$accordion_panel(
        "Scales", icon = bsi$bs_icon("rulers"),
        fe$radio(ns("scale"), "Pick scale", choices = c("A", "B", "C")),
        fe$radio(ns("subscale"), "Pick subscale", choices = c("A1", "A2", "A3")) # Subscale conditional on scale?
    )
}

#' @export
ui_output <- function(id, nav_title, card_title = NULL) {
    ns <- sh$NS(id)
    bsl$nav_panel(
        nav_title,
        bsl$card_title(card_title),
        bsl$card_body(e4r$echarts4rOutput(ns("plot"))),
        bsl$card_footer(
            class = "d-flex flex-row justify-content-between align-items-center",
            sh$actionButton(
                style = "border: none;",
                ns("download-default"),
                sh$div(
                    class = "d-flex gap-2 align-items-center",
                    bsi$bs_icon("download", size = "1.25rem"), "Download"
                )
            ),
            sh$div(bsi$bs_icon("lightbulb"), "Hover to see details")
        )
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))
        y <- "sop_om"

        res_interactive <- sh$reactive({
            min_x <- min(data()$year)
            max_x <- max(data()$year)
            data() %>%
                e4r$e_charts_("year") %>%
                e4r$e_effect_scatter_(y, "N") %>%
                # This would color the points after N as well
                # e4r$e_visual_map_("N", scale = e4r$e_scale) %>%
                e4r$e_tooltip(
                    formatter = htw$JS("
                        function(params){
                            return('<b>Year</b>: ' + params.value[0] + '<br /><b>Value</b>: ' + params.value[1] + '<br /><b>N</b>: ' + params.value[2])
                        }
                    "),
                    # textStyle = list(fontFamily = "Comissioner"),
                    trigger = "item"
                ) %>%
                e4r$e_legend(FALSE) %>%
                e4r$e_x_axis(min = min_x, max = max_x)
        })

        output$plot <- e4r$renderEcharts4r(res_interactive())
    })
}
