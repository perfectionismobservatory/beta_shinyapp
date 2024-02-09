box::use(
  sh = shiny,
  bsl = bslib,
  shiny.router[router_ui, router_server, route],
  googlesheets4[gs4_auth],
  googledrive[drive_token, drive_auth],
  shinyjs[useShinyjs],
  dotenv[load_dot_env],
  here[here],
  shinyFeedback[useShinyFeedback],
  vroom,
)

box::use(
  app / view / pages / home,
  app / view / pages / explore,
  app / view / pages / contribute,
  app / logic / frontend[theme_light],
)

load_dot_env(file = here(".env"))

# Authenticate Google sheets
# drive_auth(cache = ".secrets", email = Sys.getenv("EMAIL"))
# gs4_auth(token = drive_token(), email = Sys.getenv("EMAIL"))

# Load simulated data while UI testing
data <- vroom$vroom("data/simulate.csv")

#' @export
ui <- function(id) {
  ns <- sh$NS(id)
  bsl$page(
    useShinyjs(),
    useShinyFeedback(),
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
    explore$server("explore", data)
    contribute$server("contribute")
  })
}
