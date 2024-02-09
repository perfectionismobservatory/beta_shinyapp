box::use(
    sh = shiny,
    bsl = bslib,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList(
        bsl$accordion_panel(),
        bsl$accordion_panel()
        # Not sure how many panels we need, but these can then be spliced
        # into the main accordion on `explore.R`
        )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        # The server will return filtered data
    })
}
