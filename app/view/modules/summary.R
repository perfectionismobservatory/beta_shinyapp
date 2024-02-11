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
                    \(x) {
                        if (isFALSE(x)) {
                            x
                        } else {
                            !(input[[x]] %ifNAorNULL% "Unspecified") %in% c("Unspecified", "")
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

            if (pass()) {
                conditional_modal_footer <- bsl$card_footer(
                    style = "text-align: center;",
                    fe$btn_modal(
                        session$ns("confirm"),
                        label = "Confirm",
                        modal_title = "Great! Congrats!",
                        class_toggle = if (all_filled) "btn btn-default" else "btn btn-default disabled",
                        footer_confirm = "Continue",
                        footer_dismiss = NULL,
                        sh$div(
                            class = "d-flex flex-row flex-wrap justify-content-center gap-4",
                            !!!pr$map(
                                1:6,
                                \(n) sh$numericInput(
                                    session$ns(paste0("v", n)),
                                    paste("Variable", n),
                                    value = NA,
                                    width = "100px"
                                )
                            )
                        )
                    )
                )
            } else {
                conditional_modal_footer <- bsl$card_footer(
                    style = "text-align: center;",
                    fe$btn_modal(
                        session$ns("confirm"),
                        label = "Confirm",
                        modal_title = "Oh no, disappoint.",
                        class_toggle = if (all_filled) "btn btn-default" else "btn btn-default disabled",
                        footer_confirm = NULL,
                        footer_dismiss = "Home",
                        "No inputs here"
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
                    # Else, make icon and hr tag
                    !!!pr$map(
                        c(inputs_w_icons, "details"),
                        \(name) {
                            last_icon <- name == ut$tail(inputs_w_icons, 1)
                            if (name == "details") {
                                val <- if (conditionals_filled) "Complete" else "Unspecified"
                                fe$validation_summary[[name]](val, conditionals_filled)
                            } else {
                                val <- input[[name]] %ifNA% "Unspecified"
                                sh$div(class = "py-1", fe$validation_summary[[name]](val, val != "Unspecified"))
                            }
                        }
                    )
                ),
                conditional_modal_footer
                # If we turn this part into a dataentry module, we get
                # + upload interface in that module
                # + cleaner compartmentalised code
                # - we would have to forward the inpu- ...
                # why should the user be allowed to change, e.g., mean age again later?
                # Shouldn't we just log that into the final data frame already?
                # ... this would also mean that we have to forward it, though!
                # Might be worth it to have the data entry in here, and then forward all inputs to an upload module?
            )
        })
    })
}
