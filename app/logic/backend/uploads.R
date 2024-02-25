box::use(
    tbl = tibble,
    rl = rlang[`%||%`],
    dp = dplyr[`%>%`],
    tdr = tidyr,
    str = stringr,
)

#' @export
write_inputs_to_tibble <- function(input) {
    tbl$tibble(
        # artifact of saving a funny csv to drive, removing depends on whether hosted file has rownum column
        X = "",
        id = NA, # TODO should be `max(data()$id) + 1`
        authors = input$name,
        email = input$email, # TODO this column is not in original df, add empty col?
        doi_pmid_link = input$doc_id, # doi or preregistration link
        type_of_document = input$type,
        study = NA, # TODO what goes here?
        country = input$country,
        year = input$year,
        year_adj = NA, # TODO what is this?
        n_sample = input$n_sample,
        ratio_female = input$pct_female / 100,
        age = input$age,
        n_likert = NA, # TODO will probably be known through input$scale?
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
        o_mean = input$o_mean %||% NA,
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
prepare_for_append <- function(data) {
    data %>%
        tdr$pivot_longer(dp$contains(c("mean", "sd"))) %>%
        dp$filter(!is.na(value)) %>%
        tdr$separate_wider_delim(name, delim = "_", names = c("subscale", "measure")) %>%
        tdr$pivot_longer(dp$contains("nitems"), names_to = "subscale_nitems", values_to = "n_items") %>%
        dp$filter(str$str_detect(subscale_nitems, subscale)) %>%
        dp$select(-subscale_nitems) %>%
        tdr$pivot_wider(names_from = "measure", values_from = "value") %>%
        dp$mutate(
            dp$across(c(mean, sd), \(x) x / n_items, .names = "{.col}_adj"),
            subscale = toupper(subscale)
        )
}
