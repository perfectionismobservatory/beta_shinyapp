box::use(
  sh = shiny,
  router = shiny.router,
  rl = rlang[`%||%`],
  pr = purrr,
)

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
#' Check if LHS is type(0); if TRUE return RHS, otherwise LHS
`%//%` <- function(x, y) {
    if (length(x) == 0 && !is.null(x)) y else x
}

#' @export
between <- function(lwr, x, upr) {
  lwr <= x & x <= upr
}

#' @export
is_nothing <- function(x) {
  is.na(x) || is.null(x) || x == ""
}
