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

# The plotting pipeline that the results are exported to expects
# these columns after merging `newdata` and `predictions`:

# - x axis: year_adj
# - y axis: fit
# - 97.5 CI limit: upr
# - 2.5 CI limit: lwr

# (newdata already contains `year_adj`)
