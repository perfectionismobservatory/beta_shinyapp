box::use(
  stats[lm, predict],
)

#' @export
basic_model <- function(data) {
  lm(plotvalue ~ year_adj, data = data)
}

#' @export
basic_predictions <- function(model, newdata) {
  predict(model, newdata, interval = "confidence")
}

#' @export
basic_r2 <- function(model, digits = 3) {
  round(summary(model)$r.squared, digits)
}
