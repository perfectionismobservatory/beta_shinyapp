box::use(
    sh = shiny,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$htmlOutput(ns("entrymodal"))
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        output$entrymodal <- sh$renderUI({
            fe$btn_modal(
                "dataentry",
                label = "do something",
                title = "a title"
            )
        })
    })
}