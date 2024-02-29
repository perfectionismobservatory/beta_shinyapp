box::use(
  sh = shiny,
  bsl = bslib,
  dp = dplyr[`%>%`],
  tdr = tidyr,
  shiny.router[router_ui, router_server, route],
  googlesheets4[gs4_auth, read_sheet],
  googledrive[drive_token, drive_auth],
  shinyjs[useShinyjs],
  dotenv[load_dot_env],
  here[here],
  shinyFeedback[useShinyFeedback],
  str = stringr,
  showtext[showtext_auto],
  sysfonts[font_add_google],
  gdtools[register_gfont],
  lub = lubridate,
)

box::use(
  app / view / pages / home,
  app / view / pages / explore,
  app / view / pages / contribute,
  app / logic / frontend[theme_light],
)

load_dot_env(file = here(".env"))

# Authenticate Google sheets
drive_auth(cache = ".secrets", email = Sys.getenv("EMAIL"))
gs4_auth(token = drive_token(), email = Sys.getenv("EMAIL"))

# Add the fonts needed for pdf exports
font_add_google("Roboto", "Roboto") # Are we using Roboto right now?
font_add_google("Merriweather", "Merriweather")
font_add_google("Noto Sans", "Noto Sans")
showtext_auto()

# Add fonts for interactive rendering
register_gfont("Noto Sans")
register_gfont("Merriweather")

data <- Sys.getenv("URL") %>%
  read_sheet(col_types = "_cccccccccccccccccccc", na = "NA") %>%
  dp$mutate(
    year_as_date = lub$ymd(paste0(year, "-01-01")),
    inv_var = 1 / sd^2,
  )

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
    contribute$server("contribute", data)
    # Would it make sense to download the data separately inside the contribute server and update that
    # each time a new article is uploaded?
    # This would allow us to easily prevent the same DOI being uploaded twice in the same session / concurrent sessions
  })
}
