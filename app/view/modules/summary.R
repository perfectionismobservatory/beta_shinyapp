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
        sh$htmlOutput(ns("card")),
        sh$htmlOutput(ns("feedback"))
    )
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))
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

        # `pass` is TRUE if a user meets the inclusion criteria, and FALSE otherwise
        pass <- sh$reactive({
            cond_validation <- if (input$status == "Unspecified") {
                FALSE
            } else if (input$status == "Published") {
                pr$reduce(
                    .f = `&`,
                    # This here should also check for NULLs?
                    pr$map(published_inputs, \(x) !is.na(input[[x]]) && input[[x]] != "Unspecified")
                )
            } else {
                pr$reduce(
                    .f = `&`,
                    # This here should also check for NULLs?
                    pr$map(unpublished_inputs, \(x) !is.na(input[[x]]) && input[[x]] != "Unspecified")
                )
            }
            pr$reduce(
                .f = `&`,
                c(
                    input$scale %in% c("F-MPS", "HF-MPS"),
                    input$sample == "University students",
                    be$between(18, input$age, 25) %ifNA% FALSE,
                    cond_validation
                )
            )
        })

        # Uncomment once the check is implemented
        # I also dislike that the obs_return looks different to the change page above...
        # Generic change page obs wrapper or long version for both?
        # be$obs_return(input)

        output$card <- sh$renderUI({
            # Get those inputs that have corresponding icons
            inputs_w_icons <- names(input)[names(input) %in% names(fe$validation_summary)]
            # Probably not the most elegant, but one way to check if all items are specified
            # Step 1, check conditional inputs (need this object further down)
            conditionals_filled <- pr$reduce(
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
            # Step 2, check main inputs
            all_filled <- pr$reduce(
                c(
                    pr$map(
                        inputs_w_icons,
                        # Mind the negation!
                        \(x) (input[[x]] %ifNA% "Unspecified") != "Unspecified"
                    ),
                    conditionals_filled
                ),
                .f = `&`
            )

            if (!pass() || input$doi %in% data()$doi) {
                conditional_modal_footer <- bsl$card_footer(
                    style = "text-align: center;",
                    fe$btn_modal(
                        session$ns("confirm"),
                        label = "Confirm",
                        modal_title = "Oh no, disappoint.",
                        class_toggle = if (all_filled) fe$class_button else paste(fe$class_button, "disabled"),
                        footer_confirm = NULL,
                        footer_dismiss = "Home",
                        if (!pass()) {
                            "Failed check: Did not meet inclusion criteria."
                        } else {
                            "Failed check: DOI already exists."
                        }
                    )
                )
            } else {
                conditional_modal_footer <- bsl$card_footer(
                    style = "text-align: center;",
                    fe$btn_modal(
                        session$ns("confirm"),
                        label = "Confirm",
                        modal_title = "Great! Congrats!",
                        # Could we say something like "if all filled &&  all_valid" ?
                        class_toggle = if (all_filled) fe$class_button else paste(fe$class_button, "disabled"),
                        footer_confirm = "Continue",
                        footer_dismiss = NULL,
                        sh$div(
                            class = "d-flex flex-row flex-wrap gap-4",
                            !!!pr$map(
                                c("sop_om", "sop_osd", "spp_om", "spp_osd", "oop_om", "oop_osd", "N", "female_N"),
                                \(v) sh$numericInput(
                                    session$ns(v),
                                    toupper(v),
                                    value = NA,
                                    width = "100px"
                                )
                            ),
                            # Might want a dropdown instead
                            fe$radio(session$ns("country"), label = "Pick country", choices = c("USA", "UK", "CAN"))
                        )
                    )
                )
            }

            out <- bsl$card(
                # Same height as `layout_column_wrap` in `contribute.R`
                min_height = paste0(fe$height_layoutcolumnwrap, "px"),
                bsl$card_header("Summary"),
                bsl$card_body(
                    # Iterate over all input names that have a corresponding icon-function
                    # If current name is last name, make icon only
                    # Else, make icon with padding
                    !!!pr$map(
                        c(inputs_w_icons, "details"),
                        \(name) {
                            last_icon <- name == ut$tail(inputs_w_icons, 1)
                            if (name == "details") {
                                val <- if (conditionals_filled) "Complete" else "Unspecified"
                                fe$validation_summary[[name]](val, conditionals_filled)
                            } else {
                                val <- input[[name]] %ifNA% "Unspecified"
                                sh$div(
                                    class = "py-1",
                                    fe$validation_summary[[name]](val, val != "Unspecified" && be$is_valid(input, name))
                                )
                            }
                        }
                    )
                ),
                conditional_modal_footer
            )
        })
    })
}
