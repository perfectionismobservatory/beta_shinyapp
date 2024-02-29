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

        # Return to start page when `return` button is clicked
        be$obs_return(input)

        published_inputs <- c("name", "email", "type", "pubyear", "doi")
        unpublished_inputs <- c("name", "email", "prereg")

        # This observer first checks which inputs are currently active
        # and then resets them to their starting values
        sh$observeEvent(input$reset, {
            # Get all inputs with a corresponding summary icon
            inputs_w_icons <- names(input)[names(input) %in% names(fe$validation_summary)]
            # Also get all inputs that are in the `detail` section, which depends on publication `status`
            conditional_names <- be$get_detail_inputs(input$status)
            # Reset all inputs
            pr$walk(c(inputs_w_icons, conditional_names, "confirm", "reset", "upload"), shj$reset)
        })

        # `pass` is TRUE if a user meets the inclusion criteria, and FALSE otherwise
        pass <- sh$reactive(cond_validation <- be$run_initial_check(input))

        # Set up memory objects to track values that will be rerendered
        # input$confirm will increment, but upload and reset are rerendered
        # For correctly displaying output$dataentry, we need to keep track of input$reset
        memory <- sh$reactiveValues(confirm = 0, reset = 0, return = 0, view = 0)
        pr$map(c("confirm", "reset", "return", "view"), \(x) sh$observeEvent(input[[x]], memory[[x]] <- memory[[x]] + 1))

        # Create upload form if user passed check, otherwise print a failure message
        output$dataentry <- sh$renderUI({
            # If confirm never clicked or reset / return was last clicked, do not show card
            if (1 > memory$confirm || memory$reset == memory$confirm || memory$return == memory$confirm) {
                NULL
            # Show a failure message if the user failed the initial check
            } else if (!pass() || input$doi %in% data()$doi_pmid_link %//% FALSE) {
                fe$upload_form_failure(session$ns, input, data())
                # If check successful, card contains accordion panels with inputs
            } else {
                fe$upload_form_success(session$ns, input)
            }
        })

        # Create a chr vector with the names of all active scale inputs
        # Which inputs are active depends on which scale (F-MPS, HF-MPS) the user has selected
        all_scale_inputs <- sh$reactive({
            rgx <- paste0(paste0("^", names(fe$scale_lookup[[input$scale]])), "_", collapse = "|")
            names(input)[str$str_detect(names(input), rgx)]
        })

        # Create a bool vector showing which upload inputs are not filled
        upload_field_not_filled <- sh$reactive(
            pr$map_lgl(
                c(all_scale_inputs(), "pct_female", "n_sample"),
                \(x) be$is_nothing(input[[x]])
            )
        )

        # Notify user about unfilled upload inputs
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
                    be$write_inputs_to_tibble(data()) %>% # needs data to calculate study `id`
                    be$prepare_for_append()

                # Upload and notify user
                sheet_append(Sys.getenv("URL"), new_data, sheet = 1)
                sh$showNotification("✅ Upload successful. Thank you for your contribution!")

                # Disable upload, enable view and reset
                shj$disable("upload")
                pr$walk(c("view_toggle", "view_dismiss", "view_x", "reset"), shj$enable) # TODO missing upper right close button
            }
        })

        new_plot <- sh$eventReactive(input$upload, {
            # Format new entry
            new_entry <- input %>%
                be$write_inputs_to_tibble(data()) %>%
                be$prepare_for_append() %>%
                be$create_label() %>%
                dp$mutate(
                    dp$across(c(mean_adj, sd, year), as.numeric),
                    year_as_date = lub$ymd(paste0(year, "-01-01")),
                    inv_var = 1 / sd^2,
                    id = max(data()$id, na.rm = TRUE) + 1
                )

            # Plot
            data() %>%
                dp$filter(scale == input$scale) %>% # Show plot for scale that user entered data for
                be$plot_interactive() + # First "layer" is existing data, then new points in red on top
                gir$geom_point_interactive(data = new_entry, gg$aes(size = inv_var, tooltip = lab, data_id = id), color = "red", show.legend = FALSE)
        })

        output$new_data_plot <- gir$renderGirafe({
            gir$girafe(ggobj = new_plot(), width_svg = 7, height_svg = 4)
        })
    })
}
