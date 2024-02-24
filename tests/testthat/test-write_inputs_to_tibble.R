box::use(
    be = app / logic / backend,
    tdr = tidyr,
    dp = dplyr[`%>%`],
    str = stringr,
    tt = testthat,
)

# Made-up inputs
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

# Prepare entry for appending
new_entry <- input %>%
    be$write_inputs_to_tibble() %>%
    be$prepare_for_append()

# Load existing data
target <- read.csv("data/full.csv")

# Run test
tt$test_that(
    tt$expect_equivalent(colnames(target),colnames(new_entry)),
    desc = "column names of existing df match those of new entry"
)
