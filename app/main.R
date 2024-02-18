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
  vroom,
  str = stringr,
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

data <-
  read_sheet(Sys.getenv("URL"), col_types = "_cninnnnnnincc") %>%
  tdr$pivot_longer(c(sop_om, sop_osd, spp_osd, oop_om, oop_osd, spp_om)) %>%
  tdr$separate_wider_delim(name, delim = "_", names = c("scale", "subscale")) %>%
  dp$mutate(
    author = str$str_extract(email, "[a-z]+\\.[a-z]+"),
    author = str$str_replace(author, "\\.", " "),
    author = str$str_to_title(author),
    author = paste0(author, " et al., ", year) # the pipe isn't prettier than this
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
