box::use(
    sh = shiny,
    bsl = bslib,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        sh$htmlOutput(ns("card"))
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        output$card <- sh$renderUI({
            bsl$card(
                height = "450px", # Same height as `layout_column_wrap` in `contribute.R`
                bsl$card_header("Summary"),
                bsl$card_body("List of selected inputs"),
                bsl$card_footer(style = "text-align: center;", sh$actionButton(session$ns("confirm"), "Confirm"))
            )
        })
    })
}