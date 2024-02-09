box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
    app / view / modules / plot,
    app / view / modules / filter,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        fe$row2(
            class = "row py-2 m-4 d-flex justify-content-center align-items-center",
            colwidths = list(2, 4, 4, 2),
            content = list(
                NULL,
                fe$btn_return(ns("return")),
                sh$h1("Logo", style = "text-align: end;"),
                NULL
            )
        ),
        fe$row2(
            colwidths = list(2, 8, 2),
            content = list(
                NULL,
                sh$div(
                    bsl$navset_card_tab(
                        height = 600,
                        title = "Data viewer title",
                        # TODO namespace `id`
                        id = "nav",
                        sidebar = bsl$sidebar(
                            title = "Sidebar title",
                            width = 300,
                            # TODO adjust `condition` once `id` is in module namespace
                            sh$conditionalPanel(
                                condition = "input.nav === 'Default'",
                                bsl$accordion(
                                    open = FALSE,
                                    plot$ui_input(ns("plotdefault")),
                                    !!!filter$ui(ns("filterdefault"))
                                )
                            ),
                            sh$conditionalPanel(
                                condition = "input.nav === 'Custom'",
                                bsl$accordion(
                                    open = FALSE,
                                    plot$ui_input(ns("plotcustom")),
                                    !!!filter$ui(ns("filtercustom"))
                                )
                            )
                        ),
                        plot$ui_output(ns("plotdefault"), "Default", "Default (simulated data)"),
                        plot$ui_output(ns("plotcustom"), "Custom", "Custom (simulated data)")
                    )
                ),
                NULL
            )
        )
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        be$obs_return(input)

        filtered_data_default <- filter$server("filterdefault", sh$reactive(data))
        filtered_data_custom <- filter$server("filtercustom", sh$reactive(data))

        plot$server("plotdefault", filtered_data_default)
        plot$server("plotcustom", filtered_data_custom)
    })
}
