box::use(
    sh = shiny,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
    app / view / modules / validation,
    app / view / modules / feedback,
    app / view / modules / upload,
    app / view / modules / summary,
)

# The conditional one for "type of data" should go into a separate list
# The above design is dependent on the screen width ... ?

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
            colwidths = list(2, 6, 2, 2),
            content = list(
                NULL,
                validation$ui(
                    ns("validation"),
                    !!!fe$validation_inputs
                    # Passing the popout correctly will either have us
                    # skip the last element of inputs in `validation.R`, or
                    # make another arg field
                ),
                summary$ui(ns("validation")),
                NULL
            )
        )
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        be$obs_return(input)
        validation$server("validation")
        feedback$server("validation")
        summary$server("validation")
    })
}
