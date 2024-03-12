box::use(
    sh = shiny,
    dp = dplyr[`%>%`],
    tdr = tidyr,
    lub = lubridate,
    str = stringr,
    dotenv[load_dot_env],
    here[here],
    googlesheets4[read_sheet],
)

box::use(
    be = app / logic / backend[`%//%`],
)

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        # Get values from .env file
        load_dot_env(file = here(".env"))

        # Read data
        data <- Sys.getenv("URL") %>%
            read_sheet(col_types = "_cccccccccccccccccccc", na = "NA") %>%
            dp$select(id, authors, doi_pmid_link, country, year, year_adj, n_sample, age, scale, subscale, mean_adj) %>%
            dp$mutate(
                dp$across(c(id, year, year_adj, n_sample, age, mean_adj), as.numeric),
                year_as_date = lub$ymd(paste0(year, "-01-01")),
            ) %>%
            dp$filter(subscale != "O") %>% # not using Organization subscale
            dp$mutate(
                .by = "subscale",
                z = be$standardise(mean_adj)
            ) %>%
            dp$mutate(
                .by = "id",
                z_strivings = mean(.data[["z"]][.data[["subscale"]] %in% c("PS", "SOP")], na.rm = TRUE),
                z_concerns = mean(
                    c(
                        .data[["z"]][.data[["subscale"]] == "SPP"],
                        mean(.data[["z"]][.data[["subscale"]] %in% c("COM", "DAA")], na.rm = TRUE) %//% NA
                    ),
                    na.rm = TRUE
                )
            ) %>%
            # The following back and forth pivot is not nice, happy to adjust this but could not quickly think of a solution
            tdr$pivot_wider(names_from = "subscale", values_from = "mean_adj") %>%
            tdr$pivot_longer(c(z_strivings:OOP), names_to = "subscale", values_to = "plotvalue") %>%
            dp$filter(!is.na(plotvalue)) %>%
            dp$mutate(scale = ifelse(str$str_detect(subscale, "^z_"), "HOF", scale))

            # Return data as reactive
            sh$reactive(data)
    })
}
