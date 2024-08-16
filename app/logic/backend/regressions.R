box::use(
  metafor[...],
  stats[predict, poly],
  lub = lubridate,
  dp = dplyr[`%>%`],
  utils,
)

#' @export
basic_model <- function(full_data, active_subscale) {
  data_subscale_only <- dp$filter(full_data, subscale %in% active_subscale)

  comb1 <- escalc(
    measure = "MN",
    mi = plotvalue,
    sdi = sd_adj,
    ni = n_sample,
    data = data_subscale_only,
    append = TRUE
  )

  # Fit quadratic model only for concerns or SPP, linear for all other subscales or combined
  if (length(unique(active_subscale)) > 1) {
    fit <- rma(yi, vi, mods = ~ comb1$year_adj, data = comb1)
  } else if (unique(active_subscale) %in% c("z_concerns", "SPP")) {
    fit <- rma(yi, vi, mods = ~ comb1$centered_year + I(comb1$centered_year^2), data = comb1)
  } else {
    fit <- rma(yi, vi, mods = ~ comb1$year_adj, data = comb1)
  }

  fit
}

#' @export
basic_predictions <- function(model, subscale, raw_xs, centered_xs) {
  # Predict quadratic only for concerns or SPP, linear for all other subscales or combined
  if (length(unique(subscale)) > 1) {
    predict(model, newmods = raw_xs)
  } else if (unique(subscale) %in% c("z_concerns", "SPP")) {
    predict(model, newmods = poly(centered_xs, degree = 2, raw = TRUE))
  } else {
    predict(model, newmods = raw_xs)
  }
}

#' @export
decimal_to_date <- function(decimal_year) {
  year <- floor(decimal_year)
  days_in_year <- 365
  day_of_year <- round((decimal_year - year) * days_in_year)

  date <- lub$ymd(paste0(year, "-01-01")) + lub$days(day_of_year - 1)
  return(date)
}
