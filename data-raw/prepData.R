# Store Rda versions of raw csv files in data folder

# alprozolam
alprozolam <- read.csv("data-raw/alprazolam.csv")
usethis::use_data(alprozolam, overwrite = TRUE)

# bp_data
bp_data <- read.csv("data-raw/bp_data.csv")
usethis::use_data(bp_data, overwrite = TRUE)

# gcse_data
gcse_data <- read.csv("data-raw/gcse_data.csv")
usethis::use_data(gcse_data, overwrite = TRUE)

# incentives
incentives <- read.csv("data-raw/incentives.csv")
usethis::use_data(incentives, overwrite = TRUE)

# incontinence
incontinence <- read.csv("data-raw/incontinence.csv")
usethis::use_data(incontinence, overwrite = TRUE)

# international-airline-passengers
international_airline_passengers <- read.csv("data-raw/international-airline-passengers.csv")
usethis::use_data(international_airline_passengers, overwrite = TRUE)

# smoking_data
smoking_data <- read.csv("data-raw/smoking_data.csv")
usethis::use_data(smoking_data, overwrite = TRUE)

# uk_deaths_by_sex
uk_deaths_by_sex <- read.csv("data-raw/uk-deaths-by-sex.csv")
usethis::use_data(uk_deaths_by_sex, overwrite = TRUE)

# yrs2015
yrs2015 <- read.csv("data-raw/yrs2015.csv")
usethis::use_data(yrs2015, overwrite = TRUE)

