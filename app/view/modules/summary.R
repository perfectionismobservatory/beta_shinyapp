box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    ut = utils,
    pr = purrr,
    str = stringr,
    shj = shinyjs, # can probably remove this later
)

box::use(
    fe = app / logic / frontend,
    be = app / logic / backend[`%ifNA%`],
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
        # `pass` is TRUE if a user meets the inclusion criteria, and FALSE otherwise
        pass <- sh$reactive({
            cond_validation <- if (input$status == "Unspecified") {
                FALSE
            } else if (input$status == "Published") {
                pr$reduce(
                    .f = `&`,
                    pr$map(c("name", "email", "type", "pubyear", "doi"), \(x) !is.na(x) && x != "Unspecified")
                )
            } else {
                pr$reduce(
                    .f = `&`,
                    pr$map(c("name", "email", "prereg"), \(x) !is.na(x) && x != "Unspecified")
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
            all_filled <- pr$reduce(
                pr$map(inputs_w_icons, \(x) (input[[x]] %ifNA% "Unspecified") != "Unspecified"),
                `&`
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
                            sh$numericInput(session$ns("v1"), "Variable 1", value = NA, width = "100px"),
                            sh$numericInput(session$ns("v2"), "Variable 2", value = NA, width = "100px"),
                            sh$numericInput(session$ns("v3"), "Variable 3", value = NA, width = "100px"),
                            sh$numericInput(session$ns("v4"), "Variable 4", value = NA, width = "100px"),
                            sh$numericInput(session$ns("v5"), "Variable 5", value = NA, width = "100px"),
                            sh$numericInput(session$ns("v6"), "Variable 6", value = NA, width = "100px")
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
                        inputs_w_icons,
                        \(name) {
                            val <- input[[name]] %ifNA% "Unspecified"
                            if (name == ut$tail(inputs_w_icons, 1)) {
                                fe$validation_summary[[name]](val, val != "Unspecified")
                            } else {
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
