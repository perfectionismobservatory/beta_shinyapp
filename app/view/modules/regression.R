box::use(
  sh = shiny,
  stats[lm, predict],
  dp = dplyr[`%>%`],
  lub = lubridate,
  utils[head],
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  sh$tagList(
    sh$p(
      sh$span() # output R2 here as ... text?!
    )
  )
}

#' @export
server <- function(id, data) {
  sh$moduleServer(id, function(input, output, session) {
    stopifnot(sh$is.reactive(data))

    # Fit model
    model <- sh$reactive({
      sh$req(nrow(data()) > 0)
      lm(plotvalue ~ year_adj, data = data())
    })

    # Create some pseudo data to generate predictions for
    new_data <- sh$reactive({
      lwr_year <- if (nrow(data()) > 0) min(data()$year_adj, na.rm = TRUE) else 1981
      upr_year <- if (nrow(data()) > 0) max(data()$year_adj, na.rm = TRUE) else 2020
      data.frame(year_adj = lwr_year:upr_year)
    })

    # Get predictions and 95% CIs
    predictions <- sh$reactive(
      predict(model(), newdata = new_data(), interval = "confidence")
    )

    # Return combined pseudo data with predictions
    # The plotting pipeline that follows expects the following columns:
    # - x axis: year_adj
    # - y axis: fit
    # - 97.5 CI limit: upr
    # - 2.5 CI limit: lwr
    plot_data <- sh$reactive({
      cbind(new_data(), predictions()) %>%
        dp$mutate(
          year_as_date = lub$ymd(paste0(year_adj, "-01-01")),
        )
    })
  })
}
