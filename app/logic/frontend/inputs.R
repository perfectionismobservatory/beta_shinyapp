box::use(
    sh = shiny,
    shw = shinyWidgets
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

#' @export
radio <- function(id, label, choices, selected = NULL) {
    shw$prettyRadioButtons(
                id,
                label = label,
                choices = choices,
                selected = selected,
                status = "primary",
                shape = "curve",
                animation = "smooth",
                outline = TRUE
            )
}
