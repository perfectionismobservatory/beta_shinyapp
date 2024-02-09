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
            "Demography",
            icon = bsi$bs_icon("person-bounding-box"),
            shw$numericRangeInput(
                ns("year"),
                "Select years",
                value = c(1988, lub$year(lub$today()))
            ),
            sh$selectizeInput( # eventually replace this with shinyWidgets
                ns("country"),
                "Select countries",
                choices = c("USA", "CAN", "UK"),
                selected = c("USA", "CAN", "UK"),
                multiple = TRUE
            )
        ),
        bsl$accordion_panel(
            "Surprise",
            icon = bsi$bs_icon("balloon"),
            fe$radio(ns("dino"), "Favourite dinosaur?", choices = c("Ankylosaurus", "Quetzalcoatlus", "Iguanodon"))
        )
        # Not sure how many panels we need, but these can then be spliced
        # into the main accordion on `explore.R`
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))
        sieve <- be$filter_inputs(data, input)
        sh$reactive(data()[sieve(), ])
    })
}
