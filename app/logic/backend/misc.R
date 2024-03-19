box::use(
  sh = shiny,
  router = shiny.router,
  rl = rlang[`%||%`],
  pr = purrr,
  stats,
)

#' @export
standardise <- function(x) (x - mean(x, na.rm = TRUE)) / stats$sd(x, na.rm = TRUE)

#' @export
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

#' @export
obs_return <- function(input) {
  sh$observeEvent(input$return, {
    router$change_page("/")
  })
}

#' @export
`%ifNA%` <- function(x, y) {
  if (is.na(x)) y else x
}

#' @export
`%ifNAorNULL%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else if (is.na(x)) {
    return(y)
  } else {
    return(x)
  }
}

#' @export
between <- function(lwr, x, upr) {
  lwr <= x & x <= upr
}

#' @export
is_nothing <- function(x) {
  is.na(x) || is.null(x) || x == "" || is.nan(x)
}

#' @export
#' Check if LHS is nothing-like; if TRUE return RHS, otherwise LHS
`%//%` <- function(x, y) {
    if (is_nothing(x) || length(x) == 0) y else x
}
