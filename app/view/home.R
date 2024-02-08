box::use(
    sh = shiny,
    pr = purrr,
    router = shiny.router,
)

box::use(
    fe = app / logic / frontend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        sh$div(
            class = "container-fluid",
            fe$row2(
                colwidths = list(1, 10, 1),
                content = list(
                    NULL,
                    sh$div(
                        sh$div(
                            class = "d-flex flex-column justify-content-center align-items-center text-center mt-5",
                            sh$h1("Perfectionism repository", style = "margin-bottom: 4rem;")
                        ),
                        sh$div(
                            class = "d-flex flex-row justify-content-center",
                            style = "gap: 4rem;",
                            sh$actionButton(
                                ns("explore"),
                                sh$p(
                                    sh$h3("View"),
                                    sh$p(class = "fs-m", "perfectionism data")
                                ),
                                style = "padding: 2rem 5rem;"
                            ),
                            sh$actionButton(
                                ns("upload"),
                                sh$p(
                                    sh$h3("Contribute"),
                                    sh$p(class = "fs-m", "perfectionism data")
                                ),
                                style = "padding: 2rem 5rem;"
                            )
                        )
                    ),
                    NULL
                )
            )
        )
    )
}

#' @export
server <- function(id, pages) {
    sh$moduleServer(id, function(input, output, session) {
        wrap_change_page <- function(page) {
            sh$observeEvent(input[[page]], {
                router$change_page(page)
            })
        }

        pr$map(pages, \(page) wrap_change_page(page))
    })
}
