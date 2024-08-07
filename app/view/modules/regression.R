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
server <- function(id, full_data, filtered_data) {
  sh$moduleServer(id, function(input, output, session) {
    stopifnot(sh$is.reactive(full_data))
    stopifnot(sh$is.reactive(filtered_data))

    # Fit model
    fit <- sh$reactive({
      sh$req(nrow(filtered_data()) > 0)
      subscale <- unique(filtered_data()$subscale)
      be$basic_model(full_data(), subscale)
    })

    # Create a sequence of x values to generate predictions for
    xs <- sh$reactive({
      sh$req(nrow(filtered_data()) > 0)
      lwr_year <- min(filtered_data()$year_adj, na.rm = TRUE)
      upr_year <- max(filtered_data()$year_adj, na.rm = TRUE)
      seq(lwr_year, upr_year, length = 250)
    })

    # Get predictions and 95% CIs
    predictions <- sh$reactive({
      sh$req(length(unique(filtered_data()$subscale)) > 0)
      as.data.frame(be$basic_predictions(fit(), filtered_data()$subscale, xs()))
    })

    r2 <- sh$reactive(
      round(fit()$R2, digits = 1)
    )

    output$r2 <- sh$renderUI({
      sh$p(
        sh$span(
          paste0("The R-squared value of the model is ", r2(), ".")
        )
      )
    })

    # Return combined pseudo data with predictions
    plot_data <- sh$reactive({
      dp$mutate(predictions(), year_as_date = be$decimal_to_date(xs()))
    })
  })
}
