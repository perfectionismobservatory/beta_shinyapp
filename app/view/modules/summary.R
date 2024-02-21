box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    ut = utils,
    pr = purrr,
    str = stringr,
    shj = shinyjs, # can probably remove this later
    rl = rlang[`%||%`],
    shf = shinyFeedback,
    lub = lubridate,
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend[`%ifNA%`, `%ifNAorNULL%`],
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$div(
        bsl$card(
            # Same height as `layout_column_wrap` in `contribute.R`
            height = paste0(fe$height_layoutcolumnwrap, "px"),
            max_height = paste0(fe$height_layoutcolumnwrap, "px"),
            bsl$card_header("Summary"),
            sh$htmlOutput(ns("card")),
            bsl$card_footer(
                style = "text-align: center;",
                sh$actionButton(class = fe$class_button, ns("confirm"), "Confirm")
            )
        ),
        sh$htmlOutput(ns("feedback"))
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        # We'll need these a few times downstream, we'll wrap them here
        # Might eventually want to move this to a `misc` file or so
        published_inputs <- c("name", "email", "type", "pubyear", "doi")
        unpublished_inputs <- c("name", "email", "prereg")

        active_conditional_inputs <- sh$reactive({
            if (input$status == "Unspecified") {
                NULL
            } else if (input$status == "Published") {
                published_inputs
            } else {
                unpublished_inputs
            }
        })

        # Uncomment once the check is implemented
        # I also dislike that the obs_return looks different to the change page above...
        # Generic change page obs wrapper or long version for both?
        # be$obs_return(input)

        # Get those inputs that have corresponding icons
        inputs_w_icons <- sh$reactive(names(input)[names(input) %in% names(fe$validation_summary)])
        # Step 1, check conditional inputs (need this object further down)
        conditionals_filled <- sh$reactive(
            pr$reduce(
                pr$map(
                    active_conditional_inputs() %||% FALSE,
                    \(name) {
                        if (isFALSE(name)) {
                            name
                        } else {
                            !(input[[name]] %ifNAorNULL% "Unspecified") %in% c("Unspecified", "") && be$is_valid(input, name)
                        }
                    }
                ),
                .f = `&`
            )
        )
        # Step 2, check main inputs
        all_filled <- sh$reactive(
            pr$reduce(
                c(
                    pr$map(
                        inputs_w_icons(),
                        # Mind the negation!
                        \(x) (input[[x]] %ifNA% "Unspecified") != "Unspecified"
                    ),
                    conditionals_filled()
                ),
                .f = `&`
            )
        )

        sh$observe(shj$toggleState("confirm", condition = all_filled()))

        # Probably not the most elegant, but one way to check if all items are specified
        sh$observeEvent(input$confirm, {
            pr$map(unique(c(active_conditional_inputs(), inputs_w_icons())), \(x) shj$disable(x))
        })

        sh$observeEvent(input$reset, {
            pr$map(unique(c(active_conditional_inputs(), inputs_w_icons())), \(x) shj$enable(x))
        })

        output$card <- sh$renderUI({
            # Iterate over all input names that have a corresponding icon-function
            # If current name is last name, make icon only
            # Else, make icon with padding
            bsl$card_body(
                padding = 0,
                !!!pr$map(
                    c(inputs_w_icons(), "details"),
                    \(name) {
                        last_icon <- name == ut$tail(inputs_w_icons(), 1)
                        if (name == "details") {
                            val <- if (conditionals_filled()) "Complete" else "Unspecified"
                            fe$validation_summary[[name]](val, conditionals_filled())
                        } else {
                            # If current input is nothing-like, return "Unspecified", else the value
                            val <- ifelse(be$is_nothing(input[[name]]), "Unspecified", input[[name]])
                            check <- val != "Unspecified" && be$is_valid(input, name)
                            sh$div(
                                class = "py-1",
                                fe$validation_summary[[name]](val, check)
                            )
                        }
                    }
                )
            )
        })
    })
}
