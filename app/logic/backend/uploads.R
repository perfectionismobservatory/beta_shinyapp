box::use(
    tbl = tibble,
    rl = rlang[`%||%`],
    dp = dplyr[`%>%`],
    tdr = tidyr,
    str = stringr,
    pr = purrr,
    sh = shiny,
    lub = lubridate,
)

box::use(
    be = app / logic / backend[`%ifNA%`, `%ifNAorNULL%`, `%//%`],
)

format_author <- function(name) {
    name %>%
        str$str_to_title() %>%
        str$str_extract(".+(?=,)") %>%
        paste0(" et al.")
}

#' @export
write_inputs_to_tibble <- function(input, data) {
    tbl$tibble(
        # artifact of saving a funny csv to drive, removing depends on whether hosted file has rownum column
        X = "",
        id = max(data$id, na.rm = TRUE) + input$reset + 1, # first upload will be +1, second one +2, etc.
        authors = format_author(input$name),
        email = input$email, # TODO this column is not in original df, add empty col?
        date_added = as.character(lub$today()),
        doi_pmid_link = input$doc_id, # doi or preregistration link
        type_of_document = input$type,
        study = NA, # TODO what goes here?
        country = input$country,
        year = input$year,
        year_adj = NA, # TODO what is this?
        n_sample = input$n_sample,
        ratio_female = input$pct_female / 100,
        age = input$age,
        n_likert = if (input$scale == "HF-MPS") 5 else 7,
        scale = input$scale,
        # subscale column will be generated through pivoting
        sop_mean = input$sop_mean %||% NA,
        spp_mean = input$spp_mean %||% NA,
        oop_mean = input$oop_mean %||% NA,
        ps_mean = input$ps_mean %||% NA,
        pe_mean = input$pe_mean %||% NA,
        pc_mean = input$pc_mean %||% NA,
        com_mean = input$com_mean %||% NA,
        daa_mean = input$daa_mean %||% NA,
        sop_sd = input$sop_sd %||% NA,
        spp_sd = input$spp_sd %||% NA,
        oop_sd = input$oop_sd %||% NA,
        ps_sd = input$ps_sd %||% NA,
        pe_sd = input$pe_sd %||% NA,
        pc_sd = input$pc_sd %||% NA,
        com_sd = input$com_sd %||% NA,
        daa_sd = input$daa_sd %||% NA,
        o_sd = input$o_sd %||% NA,
        sop_nitems = input$sop_nitems %||% NA,
        spp_nitems = input$spp_nitems %||% NA,
        oop_nitems = input$oop_nitems %||% NA,
        ps_nitems = input$ps_nitems %||% NA,
        pe_nitems = input$pe_nitems %||% NA,
        pc_nitems = input$pc_nitems %||% NA,
        com_nitems = input$com_nitems %||% NA,
        daa_nitems = input$daa_nitems %||% NA,
        o_nitems = input$o_nitems %||% NA,
    )
}

#' @export
#' Format user-entered data for appending to existing data
prepare_for_append <- function(data) {
    tmp <- data %>%
        tdr$pivot_longer(dp$contains(c("mean", "sd"))) %>%
        dp$filter(!is.na(value)) %>%
        tdr$separate_wider_delim(name, delim = "_", names = c("subscale", "measure")) %>%
        tdr$pivot_longer(dp$contains("nitems"), names_to = "subscale_nitems", values_to = "n_items") %>%
        dp$filter(str$str_detect(subscale_nitems, paste0("^", subscale, "_.+"))) %>% # Avoid matching "com" with "o"
        dp$select(-subscale_nitems) %>%
        tdr$pivot_wider(names_from = "measure", values_from = "value") %>%
        dp$mutate(subscale = toupper(subscale))

    # If the given scores are lower than that scales max Likert score, assume adjusted value
    # and calculate raw values from adjusted values
    if ((tmp$scale[1] == "F-MPS" && all(tmp$mean <= 5)) || (tmp$scale[1] == "HF-MPS" && all(tmp$mean <= 7))) {
        # Not very elegant with the double rename, but it should do the job
        # Could probably think of an improvement eventually ...
        out <- tmp %>%
            dp$rename(mean_adj = "mean", sd_adj = "sd") %>%
            dp$mutate(dp$across(c(mean_adj, sd_adj), \(x) x * n_items, .names = "{.col}_tmp")) %>%
            dp$rename(mean = "mean_adj_tmp", sd = "sd_adj_tmp")
        # Otherwise calculate adjusted values from raw values
    } else {
        out <- dp$mutate(tmp, dp$across(c(mean, sd), \(x) x / n_items, .names = "{.col}_adj"))
    }

    # Convert numeric values to characters with two decimals to prevent google sheets converting numbers to dates
    dp$mutate(out, dp$across(dp$where(is.numeric), \(x) be$specify_decimal(x, 2)))
}

# Certain `detail` inputs are active depending on whether a user's study is published or not
published_inputs <- c("name", "email", "type", "pubyear", "doi")
unpublished_inputs <- c("name", "email", "prereg")

#' @export
#' Return names of active `detail` inputs depending on publication status
get_detail_inputs <- function(status) {
    if (status == "Unspecified") {
        NULL
    } else if (status == "Published") {
        published_inputs
    } else {
        unpublished_inputs
    }
}

#' @export
run_initial_check <- function(input) {
    # Validate conditional `detail` elements
    if (input$status == "Unspecified") {
        cond_valid <- FALSE
    } else if (input$status == "Published") {
        cond_valid <- pr$reduce(
            .f = `&`,
            # This here should also check for NULLs?
            pr$map(published_inputs, \(x) !be$is_nothing(input[[x]]) && input[[x]] != "Unspecified")
        )
    } else {
        cond_valid <- pr$reduce(
            .f = `&`,
            # This here should also check for NULLs?
            pr$map(unpublished_inputs, \(x) !be$is_nothing(input[[x]]) && input[[x]] != "Unspecified")
        )
    }
    # Validate all
    pr$reduce(
        .f = `&`,
        c(
            input$year %in% 1988:lub$year(lub$today()),
            # Input year must be between collection year and current year
            # If preregistration or unpublished, always pass this check
            (input$pubyear %||% input$year) %in% input$year:lub$year(lub$today()),
            input$scale %in% c("F-MPS", "HF-MPS"),
            input$sample == "University students",
            be$between(18, input$age, 25) %ifNA% FALSE,
            cond_valid
        )
    )
}
