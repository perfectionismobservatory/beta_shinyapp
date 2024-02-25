box::use(
    dp = dplyr[`%>%`],
    tbl = tibble,
    tdr = tidyr,
    pr = purrr,
    str = stringr,
)

# Create paths to read from and store names separately
nms <- c("fmps", "hfmps")
paths <- paste0("data/", nms, "_dat.csv")

# Read files, convert to tibble, and replace invalid characters with `_`
list_df <- pr$map(paths, \(x) {
    read.csv2(x, dec = ",") %>%
        tbl$as_tibble() %>%
        dp$rename_with(\(x) str$str_replace_all(x, "\\.+", "_")) %>%
        dp$mutate(Gen = str$str_replace(Gen, ",", ".") %>% str$str_replace(" ", "") %>% as.numeric())
}) %>%
    pr$set_names(nms) # Name lists

# TODO columns `Gen` in `fmps` is still `chr` -> spaces used to separate thousands
# Is this one needed urgently?

# Rotate columns representing scales and their values into long format for each data frame
df_fmps <- list_df$fmps %>%
    dp$select(-dp$contains("av_")) %>%
    tdr$pivot_longer(
        dp$contains(c("_SD", "_M", "_SDadj", "_Madj")),
        names_to = "subscale",
        values_to = "value"
    ) %>%
    tdr$pivot_longer(
        dp$contains(c("_Scale")),
        names_to = "subscale_nitems",
        values_to = "n_items"
    ) %>%
    tdr$separate_wider_delim(subscale, delim = "_", names = c("subscale", "measure")) %>%
    dp$mutate(
        subscale_nitems = str$str_remove(subscale_nitems, "_Scale"),
        scale = "F-MPS"
    ) %>%
    dp$filter(subscale == subscale_nitems) %>%
    tdr$pivot_wider(names_from = "measure", values_from = "value") %>%
    dp$select(-c(subscale_nitems, Comments, Gen)) %>%
    dp$rename_with(\(x) tolower(str$str_replace(x, "adj", "_adj"))) %>%
    dp$rename(n_likert = "point", n_sample = "n", mean = "m", mean_adj = "m_adj") %>%
    # Reorder columns to allow for `bind_rows`
    dp$relocate(
        id, authors, doi_pmid_link, type_of_document, study, country, year,
        year_adj, n_sample, per_female, age, n_likert, scale, subscale, n_items,
        mean, sd, mean_adj, sd_adj
    )


df_hfmps <- list_df$hfmps %>%
    dp$rename(type_of_document = "Journal_article_Thesis", n_items = "MPS_Scale") %>%
    # oop needs special treatment because some NAs are noted as "n/a"
    dp$mutate(
        dp$across(dp$contains("oop"), \(x) {
            str$str_replace(x, "n/a", "") %>%
                str$str_replace(",", ".") %>%
                as.numeric()
        }),
        Study = as.character(Study) # Study in fmps contains "3b"
    ) %>%
    tdr$pivot_longer(dp$contains(c("sop", "spp", "oop")), names_to = "subscale", values_to = "value") %>%
    dp$mutate(
        subscale = ifelse(str$str_length(subscale) == 3, paste0(subscale, "_m"), subscale),
        subscale = ifelse(str$str_detect(subscale, str$regex("_[s, m]")), paste0(subscale, "adj"), subscale),
        subscale = tolower(subscale)
    ) %>%
    tdr$separate_wider_delim(subscale, delim = "_", names = c("subscale", "measure")) %>%
    # Add in underscore after separation – before not possible because single delimiter required
    dp$mutate(
        measure = str$str_replace(measure, "adj", "_adj"),
        scale = "HF-MPS",
        subscale = toupper(subscale)
    ) %>%
    dp$select(-c(Gen, Comments)) %>%
    dp$rename_with(tolower) %>%
    tdr$pivot_wider(names_from = "measure", values_from = "value") %>%
    dp$rename(n_likert = "point", n_sample = "n", mean = "m", mean_adj = "m_adj") %>%
    # Reorder columns to allow for `bind_rows`
    dp$relocate(
        id, authors, doi_pmid_link, type_of_document, study, country, year,
        year_adj, n_sample, per_female, age, n_likert, scale, subscale, n_items,
        mean, sd, mean_adj, sd_adj
    )

df_full <- dp$bind_rows(df_fmps, df_hfmps) %>%
    dp$mutate(ratio_female = per_female / 100, .before = per_female) %>%
    dp$mutate(email = NA, .after = authors) %>%
    dp$select(-per_female)

write.csv(df_full, "data/full.csv")
