box::use(
  sh = shiny,
  stats[lm, predict],
  dp = dplyr[`%>%`],
  lub = lubridate,
)

box::use(
  be = app / logic / backend,
)

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  sh$div(
    sh$htmlOutput(ns("r2")),
  )
}

#' @export
server <- function(id, data) {
  sh$moduleServer(id, function(input, output, session) {
    stopifnot(sh$is.reactive(data))

    # Fit model
    model <- sh$reactive({
      sh$req(nrow(data()) > 0)
      be$basic_model(data())
    })

    # Create some pseudo data to generate predictions for
    new_data <- sh$reactive({
      sh$req(nrow(data()) > 0)
      lwr_year <- min(data()$year_adj, na.rm = TRUE)
      upr_year <- max(data()$year_adj, na.rm = TRUE)
      data.frame(year_adj = lwr_year:upr_year)
    })

    # Get predictions and 95% CIs
    predictions <- sh$reactive(
      be$basic_predictions(model(), new_data())
    )

    r2 <- sh$reactive(
      be$basic_r2(model())
    )

    output$r2 <- sh$renderUI({
      sh$p(
        sh$span(
          paste0("The R-squared value of the model is ", r2(), ".")
        )
      )
    })

    # TODO calculate R2 and add it to the UI

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
