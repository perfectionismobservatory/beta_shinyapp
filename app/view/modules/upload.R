box::use(
    sh = shiny,
    tbl = tibble,
    googlesheets4[sheet_append],
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        sh$observeEvent(input$confirm, {
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