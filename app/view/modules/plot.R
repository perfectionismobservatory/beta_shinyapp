box::use(
    sh = shiny,
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
        
    })
}