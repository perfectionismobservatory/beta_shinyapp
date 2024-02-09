box::use(
    app / logic / backend / misc,
    app / logic / backend / filters,
)

# Backend reexports

#' @export
obs_return <- misc$obs_return

#' @export
`%ifNA%` <- misc$`%ifNA%`

#' @export
filter_inputs <- filters$filter_inputs
