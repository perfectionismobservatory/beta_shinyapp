box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    lub = lubridate,
    shw = shinyWidgets,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        bsl$accordion_panel(
            "Scales",
            icon = bsi$bs_icon("rulers"),
            fe$radio(
                ns("view"),
                "Pick view",
                choices = c("Higher-order factors" = "hof", "MPS" = "mps"),
                selected = "mps"
            ),
            fe$radio(
                ns("scale"), # TODO lowest level for each path has to be name of filtered column
                "Pick dimension",
                choices = c("Perfectionist strivings" = "str", "Perfectionist concerns" = "con")
            ),
            sh$conditionalPanel(
                condition = "input.view == 'mps'",
                fe$radio(
                    ns("subscale"),
                    "Pick subscale",
                    choices = c(
                        "All subscales" = "all",
                        "Personal standards" = "ps",
                        "Parental expectation" = "pe",
                        "Parental criticism" = "pc",
                        "Concerns over mistakes" = "com",
                        "Doubts about actions" = "daa",
                        "Organisation" = "o"
                    )
                ),
                ns = ns
            )
        ),
        bsl$accordion_panel(
            "Demography",
            icon = bsi$bs_icon("person-bounding-box"),
            shw$numericRangeInput(
                ns("year"),
                "Select years",
                value = c(1988, lub$year(lub$today()))
            ),
            fe$checkboxgroup( # eventually replace this with shinyWidgets
                ns("country"),
                "Select countries",
                choices = c("US", "CA", "UK")
            )
        )
        # Not sure how many panels we need, but these can then be spliced
        # into the main accordion on `explore.R`
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # Update scale choices depending on chosen level 1 input
        sh$observeEvent(input$view, {
            if (input$view == "hof") {
                shw$updatePrettyRadioButtons(
                    session = session,
                    "scale",
                    choices = c("Perfectionist strivings" = "str", "Perfectionist concerns" = "con")
                )
            } else {
                shw$updatePrettyRadioButtons(
                    session = session,
                    "scale",
                    label = "Pick scale",
                    choices = c("F-MPS", "HF-MPS")
                )
            }
        })

        sh$observeEvent(input$scale, {
            if (input$scale == "F-MPS") {
                shw$updatePrettyRadioButtons(
                    session = session,
                    "subscale",
                    choices = c(
                        "All subscales" = "all",
                        "Personal standards" = "PS",
                        "Parental expectation" = "PE",
                        "Parental criticism" = "PC",
                        "Concerns over mistakes" = "COM",
                        "Doubts about actions" = "DAA",
                        "Organisation" = "O"
                    )
                )
            } else {
                shw$updatePrettyRadioButtons(
                    session = session,
                    "subscale",
                    choices = c(
                        "All subscales" = "all",
                        "Self-oriented" = "SOP",
                        "Other-oriented" = "OOP",
                        "Socially-prescribed" = "SPP"
                    )
                )
            }
        })

        sieve <- be$filter_inputs(data, input)
        sh$reactive(data()[sieve(), ])
    })
}
