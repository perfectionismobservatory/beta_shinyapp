# Let's cook up some data
box::use(
    dp = dplyr,
    tbl = tibble,
)

# Number of rows of simulated data
n_studies <- 50

# Simulate variables
countries <- sample(c("USA", "CAN", "UK"), n_studies, replace = TRUE)
year <- round(runif(n_studies, 1988, 2023), 0)
samplesize <- round(rnorm(n_studies, 350, 100), 0)
sop_om <- rnorm(n_studies, 65, 10)
sop_osd <- rnorm(n_studies, 16, 2)
spp_om <- rnorm(n_studies, 50, 5)
spp_osd <- rnorm(n_studies, 15, 5)
oop_om <- rnorm(n_studies, 55, 5)
oop_osd <- rnorm(n_studies, 10, 2.5)
female_p <- rbeta(n_studies, 5, 2)
female_n <- rbinom(n_studies, samplesize, female_p)
age <- rnorm(n_studies, 22, 2)

# Wrap all in data frame
df <- tbl$tibble(
    country = countries,
    year = year,
    N = samplesize,
    sop_om = sop_om,
    sop_osd = sop_osd,
    spp_om = spp_om,
    spp_osd = spp_osd,
    oop_om = oop_om,
    oop_osd = oop_osd,
    female = female_n,
    age = age
)

df <- dp$mutate(df, dp$across(dp$where(is.numeric), \(col) round(col, 2)))

write.csv(df, "data/simulate.csv")
