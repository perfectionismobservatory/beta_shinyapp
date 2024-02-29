box::use(
    sh = shiny,
    tbl = tibble,
    googlesheets4[sheet_append],
    bsl = bslib,
    bsi = bsicons,
    pr = purrr[`%>%`, `%||%`],
    str = stringr,
    shw = shinyWidgets,
    shj = shinyjs,
    shf = shinyFeedback,
    lub = lubridate,
    gir = ggiraph,
    gg = ggplot2,
    dp = dplyr,
)

box::use(
    be = app / logic / backend[`%ifNA%`, `%ifNAorNULL%`, `%//%`],
    fe = app / logic / frontend,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$htmlOutput(ns("dataentry"))
}

#' @export
server <- function(id, data) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(data))

        be$obs_return(input)

        published_inputs <- c("name", "email", "type", "pubyear", "doi")
        unpublished_inputs <- c("name", "email", "prereg")

        sh$observeEvent(input$reset, {
            inputs_w_icons <- names(input)[names(input) %in% names(fe$validation_summary)]
            conditional_names <- if (input$status == "Unspecified") {
                NULL
            } else if (input$status == "Published") {
                published_inputs
            } else {
                unpublished_inputs
            }
            pr$walk(c(inputs_w_icons, conditional_names, "confirm", "reset", "upload"), shj$reset)
        })

        # `pass` is TRUE if a user meets the inclusion criteria, and FALSE otherwise
        pass <- sh$reactive({
            cond_validation <- if (input$status == "Unspecified") {
                FALSE
            } else if (input$status == "Published") {
                pr$reduce(
                    .f = `&`,
                    # This here should also check for NULLs?
                    pr$map(published_inputs, \(x) !be$is_nothing(input[[x]]) && input[[x]] != "Unspecified")
                )
            } else {
                pr$reduce(
                    .f = `&`,
                    # This here should also check for NULLs?
                    pr$map(unpublished_inputs, \(x) !be$is_nothing(input[[x]]) && input[[x]] != "Unspecified")
                )
            }
            pr$reduce(
                .f = `&`,
                c(
                    input$year %in% 1988:lub$year(lub$today()),
                    # Input year must be between collection year and current year
                    # If preregistration or unpublished, always pass this check
                    (input$pubyear %||% input$year) %in% input$year:lub$year(lub$today()),
                    input$scale %in% c("F-MPS", "HF-MPS"),
                    input$sample == "University students",
                    be$between(18, input$age, 25) %ifNA% FALSE,
                    cond_validation
                )
            )
        })

        # Set up memory objects to track values that will be rerendered
        # input$confirm will increment, but upload and reset are rerendered
        # For correctly displaying output$dataentry, we need to keep track of input$reset
        memory <- sh$reactiveValues(confirm = 0, reset = 0, return = 0, view = 0)

        # Increment values when input is clicked
        sh$observeEvent(input$confirm, {
            memory$confirm <- memory$confirm + 1
        })

        # This input would reset to 0 for each new article entered by the same user
        # Consequently, the logic in
        # if (1 > memory$confirm || memory$reset == memory$confirm)
        # requires this memory structure
        sh$observeEvent(input$reset, {
            memory$reset <- memory$reset + 1
        })

        # Same applies to return button on check failure as to reset on success
        sh$observeEvent(input$return, {
            memory$return <- memory$return + 1
        })

        sh$observeEvent(input$view, {
            memory$view <- memory$view + 1
        })

        # TODO refactor the UI components contained in this card into several lists or so
        output$dataentry <- sh$renderUI({
            # If confirm never clicked or reset / return was last clicked, do not show card
            if (1 > memory$confirm || memory$reset == memory$confirm || memory$return == memory$confirm) {
                NULL
                # Show a failure message if the user failed the initial check
            } else if (!pass() || input$doi %in% data()$doi_pmid_link %//% FALSE) {
                sh$div(
                    id = "dataentry-card",
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
                            if (input$doi %in% data()$doc_id %//% FALSE) {
                                sh$tagList(
                                    sh$p("A study with the DOI you entered is already part of our data base.")
                                )
                                # Otherwise generic failure message
                            } else {
                                sh$tagList(
                                    sh$p(
                                        "Your study is not eligible for upload because
                                        it does not fulfill our inclusion criteria.
                                        Thank you for your interest in our data base."
                                    )
                                )
                            }
                        ),
                        # Footer contains a button that returns user to start page
                        bsl$card_footer(
                            style = "text-align: center;",
                            fe$btn_return(session$ns("return"), label = "Return to start page", icon = NULL)
                        )
                    )
                )
                # If check successful, card contains accordion panels with inputs
            } else {
                sh$div(
                    id = "dataentry-card",
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
                            # TODO sort according to scribble
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
                                                session$ns(v),
                                                if (v == "n_sample") "Number of participants" else "Gender (% Female)",
                                                value = NA,
                                                width = "265px"
                                            )
                                        ),
                                        fe$disabled_upload_inputs$sample("sample_upload", session$ns, input$sample),
                                        fe$disabled_upload_inputs$age("age_upload", session$ns, input$age),
                                        sh$selectizeInput(
                                            session$ns("country"),
                                            "Country",
                                            choices = c("Other", "CAN", "UK", "USA"),
                                            multiple = FALSE,
                                            options = list(`live-search` = TRUE),
                                            width = "120px"
                                        ),
                                        fe$disabled_upload_inputs$year("year_upload", session$ns, input$year)
                                    )
                                ),
                                bsl$accordion_panel(
                                    title = paste0("Scale: ", input$scale),
                                    icon = bsi$bs_icon("rulers"),
                                    sh$div(
                                        class = "d-flex flex-row flex-wrap gap-4",
                                        fe$conditional_scale_inputs(input$scale, session$ns)
                                    )
                                ),
                                bsl$accordion_panel(
                                    title = "Publication",
                                    icon = bsi$bs_icon("journal-bookmark-fill"),
                                    sh$div(
                                        class = "d-flex flex-row flex-wrap gap-4",
                                        fe$disabled_upload_inputs$status("status_upload", session$ns, input$status),
                                        fe$disabled_upload_inputs$name("name_upload", session$ns, input$name),
                                        if (input$status == "Published") {
                                            fe$disabled_upload_inputs$doi("doc_id", session$ns, input$doi)
                                        } else {
                                            fe$disabled_upload_inputs$doi("doc_id", session$ns, input$prereg)
                                        },
                                        fe$disabled_upload_inputs$type("type_upload", session$ns, input$type)
                                    )
                                )
                            )
                        ),
                        bsl$card_footer(
                            class = "d-flex gap-3 justify-content-center align-items-center",
                            sh$actionButton(
                                class = fe$class_button,
                                session$ns("upload"),
                                sh$div(
                                    class = "d-flex justify-content-center align-items-center gap-2",
                                    bsi$bs_icon("cloud-arrow-up", size = "1.25rem"),
                                    "Upload"
                                )
                            ),
                            shj$disabled(
                                sh$actionButton(
                                    session$ns("reset"),
                                    class = fe$class_button,
                                    sh$div(
                                        class = "d-flex align-items-center gap-2",
                                        bsi$bs_icon("clipboard2-plus", size = "1.25rem"), "Add another study"
                                    )
                                )
                            ),
                            shj$disabled(
                                fe$btn_modal(
                                    ids = list(
                                        raw = session$ns("view"),
                                        toggle = session$ns("view_toggle"),
                                        close = session$ns("view_close")
                                    ),
                                    label = sh$div(
                                        class = "d-flex align-items-center gap-2",
                                        bsi$bs_icon("graph-up-arrow", size = "1.25rem"), "Show graph"
                                    ),
                                    modal_title = "Your data points are shown in red",
                                    footer_confirm = NULL,
                                    footer_dismiss = "Close",
                                    class_toggle = fe$class_button,
                                    gir$girafeOutput(session$ns("new_data_plot"))
                                )
                            )
                        )
                    )
                )
            }
        })

        all_scale_inputs <- sh$reactive({
            rgx <- paste0(paste0("^", names(fe$scale_lookup[[input$scale]])), "_", collapse = "|")
            names(input)[str$str_detect(names(input), rgx)]
        })

        upload_field_not_filled <- sh$reactive(
            pr$map_lgl(
                c(all_scale_inputs(), "pct_female", "n_sample"),
                \(x) be$is_nothing(input[[x]])
            )
        )

        sh$observeEvent(pr$map(c(all_scale_inputs(), "pct_female", "n_sample"), \(x) input[[x]]), ignoreNULL = TRUE, {
            pr$map2(
                c(all_scale_inputs(), "pct_female", "n_sample"),
                upload_field_not_filled(),
                \(name, bool) {
                    sh$observeEvent(input[[name]], ignoreNULL = TRUE, {
                        shf$feedbackDanger(
                            name,
                            bool,
                            text = "Required field",
                            icon = NULL,
                            session = session,
                        )
                    })
                }
            )
        })

        sh$observeEvent(input$upload, {
            if (any(upload_field_not_filled())) {
                # Show notification and do nothing else
                sh$showNotification("❌ Upload not successful. Please fill out all fields first.")
            } else {
                # Create tibble from inputs and prepare for append
                new_data <- input %>%
                    be$write_inputs_to_tibble() %>%
                    be$prepare_for_append()

                # Upload and notify user
                sheet_append(Sys.getenv("URL"), new_data, sheet = 1)
                sh$showNotification("✅ Upload successful. Thank you for your contribution!")

                # Disable upload, enable view and reset
                shj$disable("upload")
                pr$walk(c("view_toggle", "view_close", "reset"), shj$enable) # TODO missing upper right close button
            }
        })

        new_plot <- sh$eventReactive(input$upload, {
            # Format new entry
            new_entry <- input %>%
                be$write_inputs_to_tibble() %>%
                be$prepare_for_append(to_chr = FALSE) %>%
                dp$mutate(
                    year_as_date = lub$ymd(paste0(year, "-01-01")),
                    inv_var = 1 / sd^2
                )

            # Plot
            data() %>%
                dp$filter(scale == input$scale) %>% # Show plot for scale that user entered data for
                be$plot_interactive() +
                gir$geom_point_interactive(data = new_entry, color = "red", show.legend = FALSE) # Highlight new points in red
        })

        output$new_data_plot <- gir$renderGirafe({
            gir$girafe(ggobj = new_plot(), width_svg = 7, height_svg = 4)
        })
    })
}
