box::use(
  metafor[...],
  stats[predict, poly],
  lub = lubridate,
)

#' @export
basic_model <- function(data) {
  comb1 <- escalc(measure = "MN", mi = plotvalue, sdi = sd_adj, ni = n_sample, data = data, append = TRUE)

  # Fit quadratic model only for concerns or SPP, linear for all other subscales or combined
  if (length(unique(data$subscale)) > 1) {
    fit <- rma(yi, vi, mods = ~ as.numeric(comb1$year_adj), data = comb1)
  } else if (unique(data$subscale) %in% c("z_concerns", "SPP")) {
    fit <- rma(yi, vi, mods = ~ as.numeric(comb1$year_adj) + I(as.numeric(comb1$year_adj)^2), data = comb1)
  } else {
    fit <- rma(yi, vi, mods = ~ as.numeric(comb1$year_adj), data = comb1)
  }

  fit
}

#' @export
basic_predictions <- function(model, subscale, xs) {
  # Predict quadratic only for concerns or SPP, linear for all other subscales or combined
  if (length(unique(subscale)) > 1) {
    predict(model, newmods = xs)
  } else if (unique(subscale) %in% c("z_concerns", "SPP")) {
    predict(model, newmods = unname(poly(xs, degree = 2, raw = TRUE)))
  } else {
    predict(model, newmods = xs)
  }
}

#' @export
basic_r2 <- function(model, digits = 3) {
  round(summary(model)$r.squared, digits)
}

#' @export
decimal_to_date <- function(decimal_year) {
  year <- floor(decimal_year)
  days_in_year <- 365
  day_of_year <- round((decimal_year - year) * days_in_year)

  date <- lub$ymd(paste0(year, "-01-01")) + lub$days(day_of_year - 1)
  return(date)
}
