box::use(
    sh = shiny,
    bsl = bslib,
)

box::use(
    fe = app / logic / frontend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        fe$row2(
            colwidths = list(1, 10, 1),
            content = list(
                NULL,
                sh$h1("Perfectionism Repository - View data", class = "px-3 my-3"),
                NULL
            )
        ),
        fe$row2(
            colwidths = list(1, 10, 1),
            content = list(
                NULL,
                bsl$layout_sidebar(
                    sidebar = bsl$sidebar(title = "Filters", position = "left"),
                    bsl$layout_sidebar(
                        sidebar = bsl$sidebar(title = "Extras", position = "right"),
                        "Main contents",
                        border = FALSE
                    ),
                    border_radius = FALSE,
                    fillable = TRUE,
                    class = "p-0"
                ),
                NULL
            )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {

    })
}
