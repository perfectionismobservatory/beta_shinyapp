box::use(
  sh = shiny,
  bsl = bslib,
  shiny.router[router_ui, router_server, route],
  googlesheets4[gs4_auth, read_sheet],
  googledrive[drive_token, drive_auth],
  shinyjs[useShinyjs],
  shinyFeedback[useShinyFeedback],
  showtext[showtext_auto],
  sysfonts[font_add_google],
  gdtools[register_gfont],
  waiter,
)

box::use(
  be = app / logic / backend,
  fe = app / logic / frontend,
  app / view / modules / load,
  app / view / pages / home,
  app / view / pages / explore,
  app / view / pages / contribute,
  app / logic / frontend[theme_light],
)

# Authenticate Google sheets
if (!be$is_nothing(Sys.getenv("EMAIL"))) {
  drive_auth(cache = ".secrets", email = Sys.getenv("EMAIL"))
  gs4_auth(token = drive_token(), email = Sys.getenv("EMAIL"))
}

# Add the fonts needed for pdf exports
font_add_google("Roboto", "Roboto") # Are we using Roboto right now?
font_add_google("Merriweather", "Merriweather")
font_add_google("Noto Sans", "Noto Sans")
showtext_auto()

# Add fonts for interactive rendering
register_gfont("Noto Sans")
register_gfont("Merriweather")

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    useShinyjs(),
    useShinyFeedback(),
    waiter$useWaiter(),
    waiter$waiterPreloader(
      html = fe$waiting_screen(sh$h1(style = "margin-top: 1.25rem;", "Initiating...")),
      fadeout = TRUE,
      color = "#7A9DAF"
    ),
    theme = theme_light,
    router_ui(
      route(
        "/",
        home$ui(ns("home"))
      ),
      route(
        "explore",
        explore$ui(ns("explore"))
      ),
      route(
        "contribute",
        contribute$ui(ns("contribute"))
      )
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    router_server("/")
    home$server("home", page = c("explore", "contribute"))
    data <- load$server("load")
    explore$server("explore", data)
    contribute$server("contribute", data)
    # Would it make sense to download the data separately inside the contribute server and update that
    # each time a new article is uploaded?
    # This would allow us to easily prevent the same DOI being uploaded twice in the same session / concurrent sessions
  })
}
