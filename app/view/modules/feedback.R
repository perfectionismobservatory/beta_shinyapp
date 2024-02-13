box::use(
    sh = shiny,
    shf = shinyFeedback,
    lub = lubridate,
    str = stringr,
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
        sh$observeEvent(input$year, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "year",
                be$invalid_format$year(input$year),
                text = "Must be between X and Y",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$age, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "age",
                be$invalid_format$age(input$age),
                text = "Must be between X and Y",
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

        sh$observeEvent(input$pubyear, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "pubyear",
                be$invalid_format$pubyear(input$pubyear),
                text = "Must be between X and Y",
                icon = NULL,
                session = session
            )
        })
    })
}
