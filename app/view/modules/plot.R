box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    dp = dplyr[`%>%`],
    htw = htmlwidgets,
    gg = ggplot2,
    lub = lubridate,
    gir = ggiraph,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend,
)

# Increase point size by 20% to get borders around shapes
# The bordered shapes (# 21 etc) behave strangely with legends
# ... at least for me :)

#' @export
header_ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        class = "d-flex flex-row gap-2 align-items-center",
        sh$downloadButton(
            icon = NULL,
            class = "btn btn-secondary hover bg-transparent border-0 p-2",
            ns("download"),
            sh$div(
                class = "d-flex gap-2 align-items-center",
                bsi$bs_icon("download", size = "1.25rem"), "Download"
            )
        ),
        sh$actionButton(
            class = "btn btn-secondary hover bg-transparent border-0 p-2",
            ns("customise"),
            sh$div(
                class = "d-flex gap-2 align-items-center",
                bsi$bs_icon("brush", size = "1.25rem"), "Customise"
            )
        ) %>% bsl$tooltip("Feature in development")
    )
}

#' @export
sidebar_ui <- function(id) {
    ns <- sh$NS(id)
    bsl$accordion_panel(
        "Analysis",
        icon = bsi$bs_icon("graph-up-arrow"),
        fe$toggleswitch(ns("regression"), "Toggle regression line")
    )
}

#' @export
main_ui <- function(id, card_title = NULL) {
    ns <- sh$NS(id)
    bsl$nav_panel(
        bsl$card_title(card_title),
        bsl$card_body(gir$girafeOutput(ns("plot")))
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        # For each plot, add regression lines if input$regression is TRUE
        res_static <- sh$reactive({
            if (input$regression) {
                be$plot_static(data(), alpha = 0.3) +
                    gg$geom_smooth(gg$aes(group = subscale), color = "grey20", linewidth = 1.5, se = FALSE) +
                    gg$geom_smooth(gg$aes(group = subscale, color = subscale), se = FALSE)
            } else {
                be$plot_static(data())
            }
        })

        res_interactive <- sh$reactive({
            if (input$regression) {
                be$plot_interactive(data(), background = "#f9fbfb", alpha = 0.3) +
                    gir$geom_smooth_interactive(gg$aes(group = subscale), color = "grey20", linewidth = 1.5, se = FALSE) +
                    gir$geom_smooth_interactive(gg$aes(group = subscale, color = subscale), se = FALSE)
            } else {
                be$plot_interactive(data(), background = "#f9fbfb")
            }
        })

        output$plot <- gir$renderGirafe(gir$girafe(ggobj = res_interactive(), width_svg = 7, height_svg = 4))

        output$download <- sh$downloadHandler(
            filename = \() paste(lub$today(), "perfectrepo.pdf", sep = "_"),
            content = \(file) gg$ggsave(file, res_static(), width = 7, height = 5)
        )
    })
}
