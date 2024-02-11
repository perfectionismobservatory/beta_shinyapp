box::use(
  sh = shiny,
  router = shiny.router,
  rl = rlang[`%||%`],
)

#' @export
obs_return <- function(input) {
  sh$observe(
    input$return %||% warning("Trying to add `obs_return()`, but missing an input with id `return`.")
  )
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
