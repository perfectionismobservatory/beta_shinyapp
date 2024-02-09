box::use(
    sh = shiny,
    shw = shinyWidgets,
    lub = lubridate,
    bsi = bsicons,
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

# CSS classes for inputs below
class_summary <- "d-flex flex-row align-items-center gap-3"
class_center <- "d-flex flex-row justify-content-center align-items-center"
class_header <- "d-flex gap-2 align-items-center"

#' @export
validation_icons <- list(
    year = bsi$bs_icon("calendar-date"),
    scale = bsi$bs_icon("rulers"),
    sample = bsi$bs_icon("person-bounding-box"),
    age = bsi$bs_icon("calculator"),
    status = bsi$bs_icon("send-check")
)

#' @export
#' List of anonymous functions to programmatically create the summary
validation_summary <- list(
    year = \(x) sh$div(class = class_summary, validation_icons$year, x),
    scale = \(x) sh$div(class = class_summary, validation_icons$scale, x),
    sample = \(x) sh$div(class = class_summary, validation_icons$sample, x),
    age = \(x) sh$div(class = class_summary, validation_icons$age, x),
    status = \(x) sh$div(class = class_summary, validation_icons$status, x)
)

#' @export
validation_inputs <- list(
    list( # no restriction
        header = sh$div(class = class_header, validation_icons$year, "Year of data collection"),
        body = \(ns) sh$numericInput(
            ns("year"),
            label = "Enter a value below",
            min = 1988,
            max = lub$year(lub$today()),
            value = NA,
            width = "150px",
        )
    ),
    list( # must not be other
        class = class_center,
        header = sh$div(class = class_header, validation_icons$scale, "Scale"),
        body = \(ns) radio(
            ns("scale"),
            label = NULL,
            choices = c("Unspecified", "F-MPS", "HF-MPS", "Other")
        )
    ),
    list(
        class = class_center,
        header = sh$div(class = class_header, validation_icons$status, "Publication status"),
        body = \(ns) radio(
            ns("status"),
            label = NULL,
            choices = c("Unspecified", "Published", "In review", "Unpublished")
        )
    ),
    list( # must be below 25
        header = sh$div(class = class_header, validation_icons$age, "Mean age"),
        body = \(ns) sh$numericInput(
            ns("age"),
            label = "Enter a value below",
            value = NA,
            min = 18,
            max = 100,
            width = "150px"
        )
    ),
    list( # must be uni
        class = class_center,
        header = sh$div(class = class_header, validation_icons$sample, "Sample type"),
        body = \(ns) radio(
            ns("sample"),
            label = NULL,
            choices = c("Unspecified", "University students", "General public", "Other")
        )
    )
)
