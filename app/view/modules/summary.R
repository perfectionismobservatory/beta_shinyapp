box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    ut = utils,
    pr = purrr,
    str = stringr,
    shj = shinyjs, # can probably remove this later
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend[`%ifNA%`],
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
            # Get those inputs that have corresponding icons
            inputs_w_icons <- names(input)[names(input) %in% names(fe$validation_summary)]

            out <- bsl$card(
                # Same height as `layout_column_wrap` in `contribute.R`
                min_height = paste0(fe$height_layoutcolumnwrap, "px"),
                bsl$card_header("Summary"),
                bsl$card_body(
                    # Iterate over all input names that have a corresponding icon-function
                    # If current name is last name, make icon only
                    # Else, make icon and hr tag
                    !!!pr$map(
                        inputs_w_icons,
                        \(name) {
                            val <- input[[name]] %ifNA% "Unspecified"
                            if (name == ut$tail(inputs_w_icons, 1)) {
                                fe$validation_summary[[name]](val)
                            } else {
                                sh$div(
                                    class = "py-1",
                                    fe$validation_summary[[name]](val),
                                )
                            }
                        }
                    )
                ),
                bsl$card_footer(
                    style = "text-align: center;",
                    # TODO implement input validation and then enable this button
                    sh$actionButton(class = "disabled", session$ns("confirm"), "Confirm")
                )
            )
        })
    })
}
