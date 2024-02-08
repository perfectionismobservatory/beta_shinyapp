box::use(
    sh = shiny,
    bsl = bslib,
    rl = rlang,
    pr = purrr,
    lub = lubridate,
    shw = shinyWidgets
)

box::use(
    fe = app / logic / frontend,
)

# TODO implement checking logic
# TODO add shinyFeedback if values are outside range

class_center <- "d-flex flex-row justify-content-center align-items-center"

#' @export
inputs <- list(
    list( # no restriction
        header = "Year of data collection",
        body = \(ns) sh$numericInput(
            ns("year"),
            label = "Enter a value below",
            min = 1988,
            max = lub$year(lub$today()),
            value = NA
        )
    ),
    list( # must not be other
        class = class_center,
        header = "Scale",
        body = \(ns) fe$radio(
            ns("scale"),
            label = NULL,
            choices = c("Unspecified", "F-MPS", "HF-MPS", "Other")
        )
    ),
    list(
        class = class_center,
        header = "Publication status",
        body = \(ns) fe$radio(
            ns("status"),
            label = NULL,
            choices = c("Unspecified", "Published", "In review", "Unpublished")
        )
    ),
    list( # must be below 25
        header = "Mean age",
        body = \(ns) sh$numericInput(
            ns("age"),
            label = "Enter a value below",
            value = NA,
            min = 18,
            max = 100
        )
    ),
    list( # must be uni
        class = class_center,
        header = "Sample type",
        body = \(ns) fe$radio(
            ns("sample"),
            label = NULL,
            choices = c("Unspecified", "University students", "General public", "Other")
        )
    )
)


#' @export
height_layoutcolumnwrap <- 400

#' @export
ui <- function(id, ...) {
    dots <- rl$list2(...)
    ns <- sh$NS(id)
    bsl$layout_column_wrap(
        width = 1 / 3,
        height = height_layoutcolumnwrap,
        fixed_width = TRUE,
        !!!pr$map(dots, \(e) {
            bsl$card(
                bsl$card_header(e$header),
                bsl$card_body(class = e$class, e$body(ns))
            )
        })
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {

    })
}
