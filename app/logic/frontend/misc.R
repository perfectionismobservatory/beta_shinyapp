box::use(
    waiter,
    sh = shiny,
    rl = rlang,
)

#' @export
waiting_screen <- function(...) {
  dots <- rl$list2(...)
  sh$div(
    class = "fs-4 d-flex flex-column gap-3",
    waiter$spin_solar(),
    !!!dots
  )
}