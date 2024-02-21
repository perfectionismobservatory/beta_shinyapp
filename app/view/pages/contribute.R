box::use(
    sh = shiny,
    bsi = bsicons,
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
            colwidths = list(1, 10, 1),
            content = list(
                NULL,
                sh$div(
                    class = "d-flex flex-row justify-content-between align-items-center",
                    fe$btn_return(ns("return")),
                    sh$img(src = "static/logo_light_right.png", width = "200px", style = "text-align: end;")
                ),
                NULL
            )
        ),
        fe$row2(
            colwidths = list(1, 3, 7, 1),
            content = list(
                NULL,
                summary$ui(ns("validation")),
                validation$ui(
                    ns("validation"),
                    !!!fe$validation_inputs
                    # Passing the popout correctly will either have us
                    # skip the last element of inputs in `validation.R`, or
                    # make another arg field
                ),
                NULL
            )
        ),
        fe$row2(
            colwidths = list(1, 7, 3, 1),
            content = list(
                NULL,
                upload$ui(ns("validation")),
                NULL,
                NULL
            )
        )
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        be$obs_return(input)
        validation$server("validation")
        feedback$server("validation")
        summary$server("validation")
        upload$server("validation", sh$reactive(data))
    })
}
