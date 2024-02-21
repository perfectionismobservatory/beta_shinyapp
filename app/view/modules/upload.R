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
)

box::use(
    be = app / logic / backend[`%ifNA%`, `%ifNAorNULL%`, `%//%`, ],
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
            # shj$enable("confirm")
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
        memory <- sh$reactiveValues(confirm = 0, reset = 0)

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

        output$dataentry <- sh$renderUI({
            if (1 > memory$confirm || memory$reset == memory$confirm) {
                NULL
            } else if (!pass() || input$doi %in% data()$doi) {
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
                                "Your study is not eligible for upload."
                            )
                        ),
                        bsl$card_body(
                            if (input$doi %in% data()$doi %//% FALSE) {
                                sh$tagList(
                                    sh$p("The DOI you entered is already part of our data base."),
                                    sh$p("Click the button below to return to the start page.")
                                )
                            } else {
                                sh$tagList(
                                    sh$p("We regret that your study does not fulfill our inclusion criteria.
                                         Thank you for your interest in our data base."),
                                    sh$p("Click the button below to return to the start page.")
                                )
                            }
                        ),
                        bsl$card_footer(
                            style = "text-align: center;",
                            sh$actionButton(class = fe$class_button, session$ns("send"), sh$div("Upload"))
                        )
                    )
                )
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
                                "Your study is eligible for upload!"
                            )
                        ),
                        bsl$card_body(
                            sh$p(
                                "Please fill out the fields below that apply to your study.
                                Then click the", bsi$bs_icon("cloud-arrow-up", size = "1.25rem"), "Upload button below.
                                After this, you can choose between resetting this page to add another study, or jumping 
                                to a graph highlighting your contribution."
                            ),
                            sh$div(
                                class = "d-flex flex-row flex-wrap gap-4",
                                sh$selectizeInput(
                                    session$ns("country"),
                                    "Country",
                                    choices = c("Other", "CAN", "UK", "USA"),
                                    multiple = FALSE,
                                    options = list(`live-search` = TRUE),
                                    width = "120px"
                                ),
                                !!!pr$map(
                                    c("sop_om", "sop_osd", "spp_om", "spp_osd", "oop_om", "oop_osd", "N", "female_N"),
                                    \(v) sh$numericInput(
                                        session$ns(v),
                                        toupper(str$str_replace(v, "_", " ")),
                                        value = NA,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$numericInput(
                                        session$ns("age_upload"),
                                        "Mean age",
                                        value = input$age,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$numericInput(
                                        session$ns("year_upload"),
                                        "Data collection",
                                        value = input$year,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("scale_upload"),
                                        "Scale",
                                        value = input$scale,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("doi_upload"),
                                        "DOI",
                                        value = input$doi,
                                        width = "265px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("status_upload"),
                                        "Status",
                                        value = input$status,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("sample_upload"),
                                        "Sample",
                                        value = input$sample,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("type_upload"),
                                        "Document type",
                                        value = input$type,
                                        width = "120px"
                                    )
                                ),
                                shj$disabled(
                                    sh$textInput(
                                        session$ns("name_upload"),
                                        "Author name",
                                        value = input$name,
                                        width = "120px"
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
                                ) %>% bsl$tooltip("Feature in development")
                            ),
                            shj$disabled(
                                sh$actionButton(
                                    session$ns("view"),
                                    class = fe$class_button,
                                    sh$div(
                                        class = "d-flex align-items-center gap-2",
                                        bsi$bs_icon("graph-up-arrow", size = "1.25rem"), "Show graph"
                                    )
                                )
                            )
                            # Could we have the whole if (input$upload) then reset and view buttons
                            # Action here ... ?
                        )
                    )
                )
            }
        })

        sh$observeEvent(input$upload, {
            shj$disable("upload")
            pr$walk(c("view", "reset"), shj$enable)
        })

        sh$observeEvent(input$upload, {
            new_data <- tbl$tibble(
                removethis = "", # TODO remove, just an artifact of saving a funny csv to drive
                country = input$country,
                year = input$year,
                N = input$N,
                sop_om = input$sop_om,
                sop_osd = input$sop_osd,
                spp_om = input$spp_om,
                spp_osd = input$spp_osd,
                oop_om = input$oop_om,
                oop_osd = input$oop_osd,
                female = input$female_N,
                age = input$age,
                email = input$email,
                doi = input$doi,
            )

            sheet_append(Sys.getenv("URL"), new_data, sheet = 1)
            sh$showNotification("Upload successful! 🎉")
        })
    })
}
