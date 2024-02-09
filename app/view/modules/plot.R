box::use(
    sh = shiny,
    e4r = echarts4r,
    dp = dplyr[`%>%`],
    htw = htmlwidgets,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        e4r$echarts4rOutput(ns("plot"))
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # TODO `y` should be an argument to the server function
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