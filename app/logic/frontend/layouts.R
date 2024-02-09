box::use(
    sh = shiny,
    pr = purrr,
    rl = rlang[`%||%`],
)

box::use(
    app / logic / frontend / inputs[btn_return],
)

#' @export
#' Height for the layout_column_wrap elements on the `contribute` page
height_layoutcolumnwrap <- 400

#' @export
row2 <- function(content = list(), class = NULL, colwidths = list()) {
    stopifnot(length(content) == length(colwidths))
    stopifnot(is.numeric(unlist(colwidths)))
    stopifnot(sum(unlist(colwidths)) == 12)

    out <- pr$map(seq_along(content), \(i) {
        sh$div(
            class = paste0("col-lg-", colwidths[[i]], " col-sm-12"),
            content[[i]]
        )
    })

    sh$div(
        class = class %||% "row m-4",
        out
    )
}

#' @export
head <- function(id, title) {
    row2(
        class = "row py-4 m-4 d-flex justify-content-center align-items-center",
        colwidths = list(2, 8, 2),
        content = list(
            sh$div(btn_return(id)),
            sh$h1(title),
            sh$div(
                class = "justify-content-end",
                "Logo or other"
            )
        )
    )
}
