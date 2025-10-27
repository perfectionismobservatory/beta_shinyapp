box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    rl = rlang,
    pr = purrr,
    lub = lubridate,
    shw = shinyWidgets,
    shj = shinyjs,
)

box::use(
    fe = app / logic / frontend,
)

# TODO add icons to header and remove prefix "Year: ..." from summary
# TODO implement checking logic
# TODO add shinyFeedback if values are outside range

#' @export
ui <- function(id, ...) {
    dots <- rl$list2(...)
    ns <- sh$NS(id)
    sh$div(
        bsl$layout_column_wrap(
            width = 1 / 3,
            height = fe$height_layoutcolumnwrap,
            fixed_width = TRUE,
            gap = "1.5rem",
            !!!pr$map(dots, \(e) {
                bsl$card(
                    bsl$card_header(e$header),
                    bsl$card_body(class = e$class, e$body(ns))
                )
            }),
            sh$conditionalPanel(
                condition = paste0("input['", ns("status"), "'] !== 'Unspecified'"),
                sh$htmlOutput(ns("conditionalinputs"))
            )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        output$conditionalinputs <- sh$renderUI({
            if (input$status == "Unspecified") {
                NULL
            } else if (input$status == "Published") {
                names <- c("name", "email", "type", "ethics", "pubyear", "doi")
                fe$conditional_validation_card(
                    !!!pr$map(names, \(x) fe$conditional_validation_inputs[[x]](session$ns))
                )
            } else {
                names <- c("name", "email", "type", "ethics", "prereg")
                fe$conditional_validation_card(
                    !!!pr$map(names, \(x) fe$conditional_validation_inputs[[x]](session$ns))
                )
            }
        })
    })
}
