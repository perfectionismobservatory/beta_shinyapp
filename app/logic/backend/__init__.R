box::use(
    app / logic / backend / misc,
    app / logic / backend / filters,
    app / logic / backend / validations,
    app / logic / backend / uploads,
    app / logic / backend / plots,
    app / logic / backend / regressions,
)

# Backend reexports

#' @export
obs_return <- misc$obs_return

#' @export
`%ifNA%` <- misc$`%ifNA%`

#' @export
`%ifNAorNULL%` <- misc$`%ifNAorNULL%`

#' @export
`%//%` <- misc$`%//%`

#' @export
between <- misc$between

#' @export
is_nothing <- misc$is_nothing

#' @export
filter_inputs <- filters$filter_inputs

#' @export
invalid_format <- validations$invalid_format

#' @export
specify_decimal <- misc$specify_decimal

#' @export
standardise <- misc$standardise

#' @export
is_valid <- validations$is_valid

#' @export
write_inputs_to_tibble <- uploads$write_inputs_to_tibble

#' @export
prepare_for_append <- uploads$prepare_for_append

#' @export
get_detail_inputs <- uploads$get_detail_inputs

#' @export
run_initial_check <- uploads$run_initial_check

#' @export
plot_interactive <- plots$plot_interactive

#' @export
plot_static <- plots$plot_static

#' @export
create_label <- plots$create_label

#' @export
basic_model <- regressions$basic_model

#' @export
basic_predictions <- regressions$basic_predictions

#' @export
basic_r2 <- regressions$basic_r2

#' @export
decimal_to_date <- regressions$decimal_to_date
