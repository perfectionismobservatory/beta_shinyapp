box::use(
    sh = shiny,
    dp = dplyr[`%>%`],
    tdr = tidyr,
    lub = lubridate,
    str = stringr,
    dotenv[load_dot_env],
    here[here],
    googlesheets4[read_sheet],
    utils[read.csv],
)

box::use(
    be = app / logic / backend[`%//%`],
)

#' @export
server <- function(id) {
    sh$moduleServer(id, function(input, output, session) {
        # Get values from .env file
        tryCatch(
            load_dot_env(file = here(".env")),
            error = function(e) {
                message("Error loading .env file:", e$message)
            }
        )

        if (be$is_nothing(Sys.getenv("URL"))) {
            data <- read.csv("data/test.csv")
        } else {
            data <- read_sheet(Sys.getenv("URL"), col_types = "_ccccccccccccccccccccc", na = "NA")
        }

        # Read data
        data <- data %>%
            dp$select(id, authors, date_added, doi_pmid_link, country, year, year_adj, n_sample, age, scale, subscale, mean_adj, sd_adj) %>%
            dp$mutate(
                dp$across(c(id, year, year_adj, n_sample, age, mean_adj, sd_adj), as.numeric),
                centered_year = year_adj - min(year_adj),
                year_as_date = lub$ymd(paste0(year_adj, "-01-01")),
            ) %>%
            # Drop Organization subscale and all records uploaded in the current month
            dp$filter(subscale != "O" & lub$ceiling_date(lub$ymd(date_added), "months") <= lub$today()) %>%
            dp$mutate(
                .by = "subscale",
                z = be$standardise(mean_adj)
            ) %>%
            dp$filter(abs(z) < 3.29) %>% # Drop outliers
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
            tdr$pivot_wider(names_from = "subscale", values_from = "z") %>%
            tdr$pivot_longer(c(z_strivings:OOP), names_to = "subscale", values_to = "plotvalue") %>%
            dp$filter(!is.na(plotvalue)) %>%
            dp$mutate(scale = ifelse(str$str_detect(subscale, "^z_"), "HOF", scale))

            # Return data as reactive
            sh$reactive(data)
    })
}
