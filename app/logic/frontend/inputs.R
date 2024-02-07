box::use(
    sh = shiny,
    shiny.router,
)

#' @export
btn_return <- function(id) {
    sh$actionButton(
        id,
        label = "Back",
        icon = sh$icon("angles-left"),
        # class = "hover"
    )
}
