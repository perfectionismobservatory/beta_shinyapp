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

# Extra info like author names and doi
names <- c("John", "Lucas", "Lou", "Tracy", "Ricardo", "Kassem", "Otis", "Pascal", "Jen", "Erica")
surnames <- c("Reeland", "Underbeck", "Forsman", "Laalo", "DeAntonnetti", "Venear", "Wegman", "Jahiri")
email <- tolower(
    paste0(
        sample(names, n_studies, replace = TRUE),
        ".",
        sample(surnames, n_studies, replace = TRUE),
        round(runif(n_studies, 100, 999), 0),
        "@somemail.com"
    )
)

doi <- paste0("10.", round(runif(n_studies, 1000, 99999), 0), "/abc.def")

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
    age = age,
    email = mail,
    doi = doi,
)

df <- dp$mutate(df, dp$across(dp$where(is.numeric), \(col) round(col, 2)))

write.csv(df, "data/simulate.csv")
