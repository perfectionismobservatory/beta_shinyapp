box::use(
  sh = shiny,
  bsl = bslib,
  bsi = bsicons,
  dp = dplyr[`%>%`],
  htw = htmlwidgets,
  gg = ggplot2,
  lub = lubridate,
  gir = ggiraph,
  shf = shinyFeedback,
  pr = purrr,
)

box::use(
  fe = app / logic / frontend,
  be = app / logic / backend,
  app / view / modules / regression,
  app / logic / frontend / themes,
)

# Increase point size by 20% to get borders around shapes
# The bordered shapes (# 21 etc) behave strangely with legends
# ... at least for me :)

#' @export
header_ui <- function(id) { # nolint
  ns <- sh$NS(id)
  sh$div(
    class = "d-flex flex-row gap-2 align-items-center",
    fe$toggleswitch(ns("regression"), "Regression line", value = FALSE),
    bsl$popover(
      bsi$bs_icon("info-circle"),
      title = "Regression Line",
      regression$ui(ns("regression"))
    )
  )
}


#' @export
main_ui <- function(id, card_title = NULL) {
  ns <- sh$NS(id)
  bsl$nav_panel(
    bsl$card_title(card_title),
    bsl$card_body(gir$girafeOutput(ns("plot")))
  )
}

shape_choices <- c("Circle" = 21, "Square" = 22, "Diamond" = 23, "Triangle up" = 24, "Triangle down" = 25)

inputs <- function(ns) {
  list(
    sh$textInput(ns("color1"), "Colour 1", value = themes$plot_palette[1]),
    sh$textInput(ns("color2"), "Colour 2", value = themes$plot_palette[2]),
    sh$textInput(ns("color3"), "Colour 3", value = themes$plot_palette[3]),
    sh$selectInput(ns("shape1"), "Shape 1", choices = shape_choices, selected = 21),
    sh$selectInput(ns("shape2"), "Shape 2", choices = shape_choices, selected = 22),
    sh$selectInput(ns("shape3"), "Shape 3", choices = shape_choices, selected = 23)
  )
}

#' @export
footer_ui <- function(id) {
  ns <- sh$NS(id)
  sh$div(
    class = "d-flex flex-row gap-2 align-items-center",
    fe$btn_modal(
      ns("customise"),
      label = sh$tagList(bsi$bs_icon("brush", size = "1.25rem"), "Customise"),
      modal_title = "Customisation menu",
      # Chose dismiss button for style reasons, 
      # no confirm / dismiss distinction because button always active
      footer_confirm = NULL,
      footer_dismiss = "Confirm",
      class_toggle = "btn btn-secondary hover bg-transparent border-0 p-2",
      !!!inputs(ns)
    ),
    sh$downloadButton(
      ns("download"),
      icon = NULL,
      class = "btn btn-secondary hover bg-transparent border-0 p-2",
      sh$div(
        class = "d-flex gap-2 align-items-center",
        bsi$bs_icon("download", size = "1.25rem"), "Download"
      )
    )
  )
}

#' @export
server <- function(id, data, raw_data) {
  sh$moduleServer(id, function(input, output, session) {
    stopifnot(sh$is.reactive(data))
    stopifnot(sh$is.reactive(raw_data))

    # Fit model and get data with predictions
    data_preds <- regression$server("regression", raw_data, data)
    stopifnot(sh$is.reactive(data_preds))


    # User feedback for color input
    pr$map(1:3, \(i) {
      sh$observeEvent(input[[paste0("color", i)]], ignoreNULL = TRUE, {
          shf$feedbackDanger(
              paste0("color", i),
              be$invalid_format$color(input[[paste0("color", i)]]),
              text = "Please pass a hex value with either 6 or 8 values.",
              icon = NULL,
              session = session
          )
      })
    })

    colors <- sh$reactive({
      # Test if any color inputs are invalid
      any_invalid <- any(pr$map_lgl(1:3, \(i) {
        be$invalid_format$color(input[[paste0("color", i)]])
      }))

      # Continue only with valid colors
      sh$req(!any_invalid)
      c(input$color1, input$color2, input$color3)
    })

    shapes <- sh$reactive(as.numeric(c(input$shape1, input$shape2, input$shape3)))

    # For each plot, add regression lines if input$regression is TRUE
    res_static <- sh$reactive({
      if (input$regression) {
        be$plot_static(data(), colors = colors(), shapes = shapes()) +
          gg$geom_line(data = data_preds(), gg$aes(y = pred), color = "white", linewidth = 1.2) +
          gg$geom_line(data = data_preds(), gg$aes(y = pred), color = "#554c41", linewidth = 0.6) +
          gg$geom_ribbon(data = data_preds(), gg$aes(x = year_as_date, y = pred, ymin = ci.lb, ymax = ci.ub), alpha = 0.2)
      } else {
        be$plot_static(data(), colors = colors(), shapes = shapes())
      }
    })

    res_interactive <- sh$reactive({
      if (input$regression) {
        be$plot_interactive(data(), colors = colors(), shapes = shapes(), background = "#f2f0ed") +
          gg$geom_line(data = data_preds(), gg$aes(year_as_date, pred), color = "white", linewidth = 1.2) +
          gg$geom_line(data = data_preds(), gg$aes(year_as_date, pred), color = "#554c41", linewidth = 0.6) +
          gg$geom_ribbon(data = data_preds(), gg$aes(year_as_date, pred, ymin = ci.lb, ymax = ci.ub), alpha = 0.2)
      } else {
        be$plot_interactive(data(), colors = colors(), shapes = shapes(), background = "#f2f0ed")
      }
    })

    output$plot <- gir$renderGirafe({
      gir$girafe(ggobj = res_interactive(), width_svg = 7, height_svg = 4)
    })

    output$download <- sh$downloadHandler(
      filename = \() paste(lub$today(), "perfectrepo.pdf", sep = "_"),
      content = \(file) gg$ggsave(file, res_static(), width = 7, height = 5)
    )
  })
}
