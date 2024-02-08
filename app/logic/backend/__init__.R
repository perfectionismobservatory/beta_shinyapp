box::use(
    app / logic / backend / misc,
)

# Backend reexports

#' @export
obs_return <- misc$obs_return

#' @export
`%ifNA%` <- misc$`%ifNA%`
