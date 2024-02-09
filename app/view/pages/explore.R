box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr[`%>%`],
    pr = purrr,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
    app / view / modules / plot,
    app / view / modules / filter,
)

add_main_window <- function(ns, n) {
    fe$row2(
        colwidths = list(2, 8, 2),
        content = list(
            NULL,
            sh$div(
                bsl$card(
                    height = 615,
                    bsl$card_header(
                        class = "d-flex justify-content-between align-items-center",
                        "Data viewer title",
                        sh$actionButton(
                            style = "border: none;",
                            ns(paste0("download", n)),
                            sh$div(
                                class = "d-flex gap-2 align-items-center",
                                bsi$bs_icon("palette2", size = "1.25rem"), "Customise"
                            )
                        )
                    ),
                    bsl$layout_sidebar(
                        sidebar = bsl$sidebar(
                            # Do we want a title?
                            # title = "Filter menu",
                            width = 300,
                            bsl$accordion(
                                open = FALSE,
                                plot$ui_input(ns(paste0("plot", n))),
                                !!!filter$ui(ns(paste0("filter", n)))
                            )
                        ),
                        plot$ui_output(ns(paste0("plot", n)), "Default", NULL),
                    )
                ),
                sh$div(
                    style = "text-align: center;",
                    if (n == 4) {
                        NULL
                    } else {
                        sh$actionButton(
                            class = "my-3",
                            ns(paste0("add", n)),
                            "Add window below"
                        )
                    }
                )
            ),
            NULL
        )
    )
}

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
        add_main_window(ns, 1),
        !!!pr$map(2:4, \(n) {
            sh$conditionalPanel(
                condition = paste0("input['", ns(paste0("add", n - 1)), "'] % 2"),
                add_main_window(ns, n)
            )
        })
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        be$obs_return(input)

        pr$map(paste0("add", 1:4), \(id) {
            sh$observeEvent(input[[id]], {
                if (input[[id]] %% 2) {
                    sh$updateActionButton(session, id, "Remove window below")
                } else {
                    sh$updateActionButton(session, id, "Add window below")
                }
            })
        })

        # I am slightly confused as to how / why this works below ...
        # all objects are named filtered_data, but there is no mixups
        # Do the objects carry some namespace signature?
        filtered_data <- filter$server("filter1", sh$reactive(data))
        plot$server("plot1", filtered_data)

        filtered_data <- filter$server("filter2", sh$reactive(data))
        plot$server("plot2", filtered_data)

        filtered_data <- filter$server("filter3", sh$reactive(data))
        plot$server("plot3", filtered_data)

        filtered_data <- filter$server("filter4", sh$reactive(data))
        plot$server("plot4", filtered_data)
    })
}
