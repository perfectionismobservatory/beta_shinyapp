box::use(
    sh = shiny,
    bsl = bslib,
    bsi = bsicons,
    gir = ggiraph,
    pr = purrr,
    shj = shinyjs,
)

box::use(
    app / logic / frontend / inputs[btn_return],
    app / logic / backend / misc[`%//%`],
    app / logic / frontend / inputs[...],
)

#' @export
upload_form_failure <- function(ns, input, data) {
    sh$div(
        class = "d-flex flex-column justify-content-center align-items-center",
        bsi$bs_icon("arrow-down", size = "4rem", class = "text-info mb-4"),
        bsl$card(
            min_height = "200px",
            bsl$card_header(
                sh$div(
                    class = "d-flex align-items-center gap-2",
                    bsi$bs_icon("x-circle", class = "text-danger", size = "1.5rem"),
                    "Initial data check failed"
                )
            ),
            bsl$card_body(
                # Notify if failure was due to DOI being already included in data base
                # Otherwise generic failure message
                if (input$doi %in% data$doi_pmid_link %//% FALSE) {
                    sh$p("A study with the DOI you entered is already part of our data base.")
                } else {
                    sh$p(
                        "Your study is not eligible for upload because
                        it does not fulfill our inclusion criteria.
                        Thank you for your interest in our data base."
                    )
                }
            ),
            # Footer contains a button that returns user to start page
            bsl$card_footer(
                style = "text-align: center;",
                btn_return(ns("return"), label = "Return to start page", icon = NULL)
            )
        )
    )
}

#' @export
upload_form_success <- function(ns, input) {
    sh$div(
        class = "d-flex flex-column justify-content-center align-items-center",
        bsi$bs_icon("arrow-down", size = "4rem", class = "text-info", margin = "0 0 2.25rem 0"),
        bsl$card(
            min_height = "200px",
            bsl$card_header(
                sh$div(
                    class = "d-flex align-items-center gap-2",
                    bsi$bs_icon("check2-circle", class = "text-success", size = "1.5rem"),
                    "Initial data check passed!"
                )
            ),
            bsl$card_body(
                sh$p(
                    "Your study is eligible for upload.
                                Please fill out the fields below that apply to your study.
                                Then click the", bsi$bs_icon("cloud-arrow-up", size = "1.25rem"), "Upload button below.
                                After this, you can choose between resetting this page to add another study, or jumping
                                to a graph highlighting your contribution."
                ),
                bsl$accordion(
                    open = FALSE,
                    bsl$accordion_panel(
                        title = "Sample",
                        icon = bsi$bs_icon("person-bounding-box"),
                        sh$div(
                            class = "d-flex flex-row flex-wrap gap-4",
                            !!!pr$map(
                                c("n_sample", "pct_female"),
                                # TODO add lookup for nice labels on these inputs
                                # "Number of participants", "Gender (% Female)"
                                \(v) sh$numericInput(
                                    ns(v),
                                    if (v == "n_sample") "Number of participants" else "Gender (% Female)",
                                    value = NA,
                                    width = "265px"
                                )
                            ),
                            disabled_upload_inputs$sample("sample_upload", ns, input$sample),
                            disabled_upload_inputs$age("age_upload", ns, input$age),
                            sh$selectizeInput(
                                ns("country"),
                                "Country",
                                choices = c("Other", "CAN", "UK", "USA"),
                                multiple = FALSE,
                                options = list(`live-search` = TRUE),
                                width = "120px"
                            ),
                            disabled_upload_inputs$year("year_upload", ns, input$year)
                        )
                    ),
                    bsl$accordion_panel(
                        title = paste0("Scale: ", input$scale),
                        icon = bsi$bs_icon("rulers"),
                        sh$div(
                            class = "d-flex flex-row flex-wrap gap-4",
                            conditional_scale_inputs(input$scale, ns)
                        )
                    ),
                    bsl$accordion_panel(
                        title = "Publication",
                        icon = bsi$bs_icon("journal-bookmark-fill"),
                        sh$div(
                            class = "d-flex flex-row flex-wrap gap-4",
                            disabled_upload_inputs$status("status_upload", ns, input$status),
                            disabled_upload_inputs$name("name_upload", ns, input$name),
                            if (input$status == "Published") {
                                disabled_upload_inputs$doi("doc_id", ns, input$doi)
                            } else {
                                disabled_upload_inputs$doi("doc_id", ns, input$prereg)
                            },
                            disabled_upload_inputs$type("type_upload", ns, input$type)
                        )
                    )
                )
            ),
            bsl$card_footer(
                class = "d-flex gap-3 justify-content-center align-items-center",
                sh$actionButton(
                    class = class_button,
                    ns("upload"),
                    sh$div(
                        class = "d-flex justify-content-center align-items-center gap-2",
                        bsi$bs_icon("cloud-arrow-up", size = "1.25rem"),
                        "Upload"
                    )
                ),
                shj$disabled(
                    sh$actionButton(
                        ns("reset"),
                        class = class_button,
                        sh$div(
                            class = "d-flex align-items-center gap-2",
                            bsi$bs_icon("clipboard2-plus", size = "1.25rem"), "Add another study"
                        )
                    )
                ),
                shj$disabled(
                    btn_modal(
                        id = ns("view"),
                        label = sh$div(
                            class = "d-flex align-items-center gap-2",
                            bsi$bs_icon("graph-up-arrow", size = "1.25rem"), "Show graph"
                        ),
                        modal_title = "Your data points are shown in red",
                        footer_confirm = NULL,
                        footer_dismiss = NULL,
                        class_toggle = class_button,
                        gir$girafeOutput(ns("new_data_plot"))
                    )
                )
            )
        )
    )
}
