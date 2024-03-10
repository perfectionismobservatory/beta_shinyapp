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
                        class = "d-flex flex-column justify-content-center align-items-center gap-5 p-5",
                        sh$div(
                            class = "d-flex justify-content-center align-items-center",
                            sh$img(src = "static/logo_light_centered.png", width = "400px")
                        ),
                        sh$div(
                            class = "d-flex flex-column justify-content-center text-secondary align-items-center",
                            sh$h3(
                                style = "max-width: 775px; margin-bottom: 1.5rem; text-align: center;",
                                "A global database and visualisation tool for perfectionism data"
                            ),
                            sh$h6(
                                style = "max-width: 775px; text-align: left; margin: 0; letter-spacing: 0.02rem; line-height: 140%;",
                                "How is perfectionism changing over time? This tool provides a real-time, open access
                                database to track levels of perfectionism among young people based on the Frost and Hewitt-Flett
                                Multidimensional Perfectionism Scales.ᵃ ᵇ"
                            )
                        ),
                        sh$div(
                            class = "d-flex flex-row justify-content-center align-items-center",
                            style = "gap: 4rem;",
                            sh$actionButton(
                                ns("explore"),
                                class = paste(fe$class_button, "px-4 py-2"),
                                sh$p(
                                    sh$h3(style = "margin: 0", "View"),
                                    sh$p(class = "fs-m", "Perfectionism data")
                                )
                            ),
                            sh$actionButton(
                                ns("contribute"),
                                class = paste(fe$class_button, "px-4 py-2"),
                                sh$p(
                                    sh$h3(style = "margin: 0", "Contribute"),
                                    sh$p(class = "fs-m", "Perfectionism data")
                                )
                            )
                        ),
                        sh$div(
                            class = "d-flex flex-column justify-content-center align-items-center gap-3",
                            sh$div(
                                class = "text-secondary",
                                sh$p(
                                    style = "font-size: 7pt; text-align: center; max-width: 600px;",
                                    "a - Frost, R. O., Marten, P., Lahart, C., & Rosenblate, R. (1990). The dimensions of perfectionism.
                                Cognitive Therapy and Research, 14(5), 449–468. https://doi.org/10.1007/BF01172967"
                                ),
                                sh$p(
                                    style = "font-size: 7pt; text-align: center; max-width: 600px;",
                                    "b - Hewitt, P. L., & Flett, G. L. (1991). Perfectionism in the self and social contexts: Conceptualization,
                                assessment, and association with psychopathology. Journal of Personality and Social Psychology, 60(3), 456–470.
                                https://doi.org/10.1037/0022-3514.60.3.456"
                                )
                            ),
                            sh$img(
                                src = "static/uni_logos.png",
                                width = "400px",
                                style = "text-align: center;"
                            ),
                            sh$div(
                                sh$h6(
                                    style = "font-size: 9pt; text-align: center;",
                                    class = "text-secondary",
                                    "Thomas Curran¹, Pia Marie Pose¹, Andrew Hill², and Simon Steiger³"
                                ),
                                sh$p(
                                    style = "font-size: 7pt; text-align: center;",
                                    class = "text-secondary",
                                    "affiliated with ¹ London School of Economics, ² York St John, ³ Karolinska Institutet"
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
