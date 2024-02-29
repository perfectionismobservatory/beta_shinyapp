box::use(
    sh = shiny,
    shf = shinyFeedback,
    lub = lubridate,
    str = stringr,
    pr = purrr,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        # Print error feedback for invalid inputs

        sh$observeEvent(input$age, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "age",
                be$invalid_format$age(input$age),
                text = "Provide one decimal, e.g., 20.3",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$name, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "name",
                be$invalid_format$name(input$name),
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$email, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "email",
                be$invalid_format$email(input$email),
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$doi, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "doi",
                be$invalid_format$doi(input$doi),
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })
    })
}
