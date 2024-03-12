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
            sh$conditionalPanel(
                condition = "input.view == 'mps'",
                fe$radio(
                    ns("scale"),
                    label = "Pick scale",
                    choices = c("F-MPS", "HF-MPS")
                ),
                ns = ns,
            ),
            fe$radio(
                ns("subscale"),
                "Pick subscale",
                choices = c(
                    "All subscales" = "all",
                    "PS (Personal standards)" = "PS",
                    "PE (Parental expectation)" = "PE",
                    "PC (Parental criticism)" = "PC",
                    "COM (Concerns over mistakes)" = "COM",
                    "DAA (Doubts about actions)" = "DAA"
                )
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

        sh$observeEvent(input$view, {
            if (input$view == "hof") {
                # Set scale to HOF for filtering
                # This radio button is hidden in the `conditionalPanel`
                shw$updatePrettyRadioButtons(
                    session = session,
                    "scale",
                    choices = "HOF"
                )
                # Set correct "subscale" options for higher order factors
                shw$updatePrettyRadioButtons(
                    session = session,
                    "subscale",
                    choices = c("Perfectionist strivings" = "z_strivings", "Perfectionist concerns" = "z_concerns")
                )
            } else if (input$view != "hof") {
                # Set correct scale choices if HOF are not displayed
                # This radio button will be displayed
                shw$updatePrettyRadioButtons(
                    session = session,
                    "scale",
                    choices = c("F-MPS", "HF-MPS")
                )
            }
        })

        sh$observeEvent(input$scale, {
            if (input$scale == "F-MPS") {
                # Set correct subscale choices for F-MPS
                shw$updatePrettyRadioButtons(
                    session = session,
                    "subscale",
                    choices = c(
                        "All subscales" = "all",
                        "PS (Personal standards)" = "PS",
                        "PE (Parental expectation)" = "PE",
                        "PC (Parental criticism)" = "PC",
                        "COM (Concerns over mistakes)" = "COM",
                        "DAA (Doubts about actions)" = "DAA"
                    )
                )
            } else if (input$scale == "HF-MPS") {
                # Set correct subscale choices for HF-MPS
                shw$updatePrettyRadioButtons(
                    session = session,
                    "subscale",
                    choices = c(
                        "All subscales" = "all",
                        "SOP (Self-oriented)" = "SOP",
                        "OOP (Other-oriented)" = "OOP",
                        "SPP (Socially-prescribed)" = "SPP"
                    )
                )
            }
        })

        # Filter data by inputs and drop rows where at least one filter condition is not met
        sieve <- be$filter_inputs(data, input)
        sh$reactive(data()[sieve(), ])
    })
}
