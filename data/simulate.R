library(purrr)
library(here)

n <- 100
id <- 1:n
authors <- map_chr(1:n, \(x) paste0(paste0(sample(letters, 10), collapse = ""), " et al"))
email <- map_chr(1:n, \(x) paste0(paste0(sample(letters, 10), collapse = ""), "@abc.de"))
doi_pmid_link <- map_chr(1:n, \(x) paste0("10.", paste0(sample(0:9, 4), collapse = ""), "/ab.cd"))
type_of_document <- map_chr(1:n, \(x) paste(paste0(sample(letters, 5), collapse = ""), paste0(sample(letters, 5), collapse = "")))
study <- sample(1:4, n, replace = TRUE)
country <- sample(c("US", "CA", "UK"), n, replace = TRUE)
year <- round(runif(n, 1980, 2023), digits = 0)
year_adj <- year + sample(-2:2, n, replace = TRUE)
ratio_female <- rbeta(n, 2, 2)
age <- round(runif(n, 18, 24), digits = 2)
scale <- sample(c("HF-MPS", "F-MPS"), 100, replace = TRUE)
subscale <- map_chr(scale, \(x) ifelse(x == "HF-MPS", sample(c("SOO", "OOP", "SOP")), sample(c("DAA", "COM", "PS", "PC"))))
n_items <- round(runif(n, min = 4, max = 15), digits = 0)
mean <- runif(n, min = 15, max = 50)
sd <- runif(n, min = 1, max = 4)
mean_adj <- runif(n, min = 2, max = 5)
sd_adj <- runif(n, min = 0.5, max = 1)

df <- data.frame(
    id = id,
    authors = authors,
    email = email,
    doi_pmid_link = doi_pmid_link,
    type_of_document = type_of_document,
    study = study,
    country = country,
    year = year,
    year_adj = year_adj,
    ratio_female = ratio_female,
    age = age,
    scale = scale,
    subscale = subscale,
    n_items = n_items,
    mean = mean,
    sd = sd,
    mean_adj = mean_adj,
    sd_adj = sd_adj
)

write.csv(df, here("data", "test.csv"))
