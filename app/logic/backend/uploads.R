box::use(
    tbl = tibble,
    rl = rlang[`%||%`],
)

#' @export
write_inputs_to_tibble <- function(input) {
    tbl$tibble(
        # artifact of saving a funny csv to drive, removing depends on whether hosted file has rownum column
        removethis = "",
        country = input$country,
        year = input$year,
        N = input$total_N,
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
        female = input$female_N,
        age = input$age,
        email = input$email,
        doi = input$doi,
    )
}
