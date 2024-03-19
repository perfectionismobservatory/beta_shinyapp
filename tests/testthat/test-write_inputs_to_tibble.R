box::use(
    be = app / logic / backend,
    tdr = tidyr,
    dp = dplyr[`%>%`],
    str = stringr,
    tt = testthat,
    tbl = tibble,
    here,
)

# Made-up inputs HF-MPS
input <- list(
    name = "Josef Tretter",
    email = "joseftretter@aol.com",
    doc_id = "www.google.com",
    type = "Journal",
    year = 2019,
    country = "US",
    n_sample = 119,
    pct_female = 51,
    age = 20.3,
    scale = "HF-MPS",
    sop_mean = 50,
    spp_mean = 44,
    oop_mean = 48,
    sop_sd = 12,
    spp_sd = 15,
    oop_sd = 17,
    sop_nitems = 15,
    spp_nitems = 15,
    oop_nitems = 15
)

# Made-up inputs F-MPS
input_fmps <- list(
    name = "Josef Tretter",
    email = "joseftretter@aol.com",
    doc_id = "www.google.com",
    type = "Journal",
    year = 2019,
    country = "US",
    n_sample = 119,
    pct_female = 51,
    age = 20.3,
    scale = "F-MPS",
    daa_mean = 50,
    com_mean = 44,
    ps_mean = 48,
    o_mean = 46,
    pc_mean = 40,
    daa_sd = 12,
    com_sd = 15,
    ps_sd = 17,
    o_sd = 14,
    pc_sd = 13,
    daa_nitems = 15,
    com_nitems = 15,
    ps_nitems = 15,
    o_nitems = 15,
    pc_nitems = 15,
    reset = 0
)

# Made-up inputs HF-MPS
input_hfmps <- list(
    name = "Josef Tretter",
    email = "joseftretter@aol.com",
    doc_id = "www.google.com",
    type = "Journal",
    year = 2019,
    country = "US",
    n_sample = 119,
    pct_female = 51,
    age = 20.3,
    scale = "HF-MPS",
    sop_mean = 50,
    spp_mean = 44,
    oop_mean = 48,
    sop_sd = 12,
    spp_sd = 15,
    oop_sd = 17,
    sop_nitems = 15,
    spp_nitems = 15,
    oop_nitems = 15,
    reset = 0
)

# Load existing data
target <- tbl$tibble(read.csv(here$here("data/test.csv")))

# Prepare entry for appending
new_entry_hfmps <- input_hfmps %>%
    be$write_inputs_to_tibble(target) %>%
    be$prepare_for_append()

new_entry_fmps <- input_fmps %>%
    be$write_inputs_to_tibble(target) %>%
    be$prepare_for_append()

# Run test
tt$test_that(
    tt$expect_equivalent(colnames(target), colnames(new_entry_hfmps)),
    desc = "column names of existing df match those of new entry"
)

tt$test_that(
    tt$expect_equivalent(colnames(target), colnames(new_entry_fmps)),
    desc = "column names of existing df match those of new entry"
)
