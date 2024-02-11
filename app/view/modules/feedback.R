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

regex_mail <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
regex_doi <- "\\b(10\\.\\d{4,5}/[-._;()/:A-Za-z0-9]+)\\b"

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
                !be$between(1900, input$year, lub$year(lub$today())),
                text = "Must be between X and Y",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$age, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "age",
                !be$between(18, input$age, 100),
                text = "Must be between X and Y",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$name, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "name",
                !str$str_detect(input$name, "\\w+\\, \\w+") && input$name != "",
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$email, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "email",
                !str$str_detect(input$email, regex_mail) && input$email != "",
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$doi, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "doi",
                !str$str_detect(input$doi, regex_doi) && input$doi != "",
                text = "Incorrect format",
                icon = NULL,
                session = session
            )
        })

        sh$observeEvent(input$pubyear, ignoreNULL = TRUE, {
            shf$feedbackDanger(
                "pubyear",
                !be$between(1900, input$pubyear, lub$year(lub$today())),
                text = "Must be between X and Y",
                icon = NULL,
                session = session
            )
        })
    })
}
