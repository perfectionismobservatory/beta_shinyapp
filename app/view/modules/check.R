box::use(
    sh = shiny,
    bsl = bslib,
    rl = rlang,
    pr = purrr,
)

#' @export
ui <- function(id, ...) {
    dots <- rl$list2(...)
    ns <- sh$NS(id)
    bsl$layout_column_wrap(
        width = 1 / 3,
        height = 450,
        fixed_width = TRUE,
        !!!pr$map(dots, \(e) {
            bsl$card(
                min_height = "150px",
                bsl$card_header(e$header),
                bsl$card_body(e$body)
            )
        })
    )
}

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        
    })
}