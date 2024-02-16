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
                colwidths = list(2, 8, 2),
                content = list(
                    NULL,
                    sh$div(
                        sh$div(
                            class = "d-flex justify-content-center align-items-center my-5",
                            sh$img(
                                src = "static/logo_light_centered.png",
                                width = "400px",
                                style = "margin-bottom: 1.5rem;"
                            )
                        ),
                        sh$div(
                            class = "d-flex flex-row justify-content-center",
                            style = "gap: 4rem;",
                            sh$actionButton(
                                ns("explore"),
                                class = paste(fe$class_button, "px-5 py-3"),
                                sh$p(
                                    sh$h3("View"),
                                    sh$p(class = "fs-m", "perfectionism data")
                                )
                            ),
                            sh$actionButton(
                                ns("contribute"),
                                class = paste(fe$class_button, "px-5 py-3"),
                                sh$p(
                                    sh$h3("Contribute"),
                                    sh$p(class = "fs-m", "perfectionism data")
                                )
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
