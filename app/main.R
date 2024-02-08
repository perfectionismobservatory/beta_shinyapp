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
)

box::use(
  app / view / home,
  app / view / explore,
  app / view / upload,
  app / logic / frontend[theme_light],
)

load_dot_env(file = here(".env"))

# Authenticate Google sheets
# drive_auth(cache = ".secrets", email = Sys.getenv("EMAIL"))
# gs4_auth(token = drive_token(), email = Sys.getenv("EMAIL"))

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
        "upload",
        upload$ui(ns("upload"))
      )
    )
  )
}

#' @export
server <- function(id) {
  sh$moduleServer(id, function(input, output, session) {
    router_server("/")
    home$server("home", page = c("explore", "upload"))
    explore$server("explore")
    upload$server("upload")
  })
}
