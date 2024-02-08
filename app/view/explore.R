box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
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
                bsl$navset_card_tab(
                    height = 600,
                    title = "Data viewer title",
                    # TODO namespace `id`
                    id = "nav",
                    sidebar = bsl$sidebar(
                        title = "Sidebar title",
                        # TODO adjust `condition` once `id` is in module namespace
                        sh$conditionalPanel(
                            condition = "input.nav === 'Plot A'",
                            "Page 1 sidebar"
                        ),
                        sh$conditionalPanel(
                            condition = "input.nav === 'Plot B'",
                            "Page 2 sidebar"
                        )
                    ),
                    bsl$nav_panel(
                        "Plot A",
                        bsl$card_title("Default plot"),
                        bsl$card_footer(
                            class = "d-flex flex-row justify-content-between align-items-center",
                            sh$actionButton(ns("download-default"), "Download A"),
                            "Note or icon A"
                        ),
                    ),
                    bsl$nav_panel(
                        "Plot B",
                        bsl$card_title("Custom plot"),
                        bsl$card_footer(
                            class = "d-flex flex-row justify-content-between align-items-center",
                            sh$actionButton(ns("download-custom"), "Download B"),
                            "Note or icon B"
                        ),
                    )
                ),
                NULL
            )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        be$obs_return(input)
    })
}
