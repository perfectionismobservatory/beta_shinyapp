box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    ut = utils,
    pr = purrr,
    str = stringr,
)

box::use(
    be = app / logic / backend[`%ifNA%`],
    app / view / modules / check,
)

# CSS class for all summary items
summary_class <- "d-flex flex-row align-items-center gap-3"

# List of anonymous functions to programmatically create the summary
icons <- list(
    year = \(x) sh$div(class = summary_class, bsi$bs_icon("calendar-date"), x),
    scale = \(x) sh$div(class = summary_class, bsi$bs_icon("rulers"), x),
    sample = \(x) sh$div(class = summary_class, bsi$bs_icon("person-bounding-box"), x),
    age = \(x) sh$div(class = summary_class, bsi$bs_icon("calculator"), x),
    status = \(x) sh$div(class = summary_class, bsi$bs_icon("send-check"), x)
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        sh$htmlOutput(ns("card"))
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        output$card <- sh$renderUI({
            # Get those inputs that have corresponding icons
            inputs_w_icons <- names(input)[names(input) %in% names(icons)]

            bsl$card(
                # Same height as `layout_column_wrap` in `contribute.R`
                min_height = paste0(check$height_layoutcolumnwrap, "px"),
                bsl$card_header("Summary"),
                bsl$card_body(
                    # Iterate over all input names that have a corresponding icon-function
                    # If current name is last name, make icon only
                    # Else, make icon and hr tag
                    !!!pr$map(
                        inputs_w_icons,
                        \(name) {
                            val <- input[[name]] %ifNA% "Unspecified"
                            if (name == ut$tail(inputs_w_icons, 1)) {
                                icons[[name]](paste0(str$str_to_title(name), ": ", val))
                            } else {
                                sh$div(
                                    class = "py-1",
                                    icons[[name]](paste0(str$str_to_title(name), ": ", val)),
                                )
                            }
                        }
                    )
                ),
                bsl$card_footer(style = "text-align: center;", sh$actionButton(session$ns("confirm"), "Confirm"))
            )
        })
    })
}