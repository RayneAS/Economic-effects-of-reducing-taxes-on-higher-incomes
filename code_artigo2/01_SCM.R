#obs:Inequality data it was cleaned and organized by Mariana in another code 

rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "haven",
  "Synth"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))

#PACKAGES USED
library(data.table)
library(readr)
library(haven)

# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"

  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")


# 1 - Open Income database (World Inequality Database) -------------------------


dt_income <- data.table(
  read_csv(
    file.path(data_dir, "final_data_inequality_WID.csv")))

colnames(dt_income)
dt_income <- dt_income[, ("Code") := NULL]


dt_income[, year := as.integer(year)]

setorder(dt_income, Country, year)

unique_country <- unique(dt_income$Country)
unique_country


# 2 - Filter countries and years------------------------------------------------

#at first consider a donor pool with South Africa, Colombia, Chile, 
#and Mexico

#Brazil
#South Africa
#Colombia
#Chile
#Mexico

dt_income <- dt_income[ Country %in% c("South Africa","Colombia",
  "Chile","Mexico","Brazil")]

#filter years

dt_income <- dt_income[ year >= 1980]

unique_year <- length(unique(dt_income$year))
unique_year

#check final sample
dt_income[, .(
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE),
  n_years  = uniqueN(year)
), by = Country]


# 3- Treatment timing ----------------------------------------------------------
treat_year <- 1996

dt_income[, treated := as.integer(Country == "Brazil")]
dt_income[, post := as.integer(year >= treat_year)]
dt_income[, pre := as.integer(year < treat_year)]

#check final sample
dt_income[, .(
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE),
  n_years = uniqueN(year),
  n_pre = sum(year < treat_year),
  n_post = sum(year >= treat_year)
), by = Country]


# 4 - Main Outcome -------------------------------------------------------------

outcome_var <- "gini_post_tax"
outcome_robust <- "d_share_top1"

dt_income[, y := get(outcome_var)]

#check final sample for outcome 
dt_income[, .(
  min_year = min(year, na.rm = TRUE),
  max_year = max(year, na.rm = TRUE),
  n_years  = uniqueN(year),
  n_pre    = sum(year < treat_year),
  n_post   = sum(year >= treat_year),
  n_missing_y = sum(is.na(y))
), by = Country]

#check pre-treatment outcome availability
dt_income[year < treat_year, .(
  n_pre = .N,
  n_pre_nonmissing = sum(!is.na(get(outcome_var))),
  first_pre_year = min(year[!is.na(get(outcome_var))], na.rm = TRUE),
  last_pre_year  = max(year[!is.na(get(outcome_var))], na.rm = TRUE)
), by = Country]


# 5 - organize data to run SCM--------------------------------------------------

#create numeric country id
dt_income[, country_id := .GRP, by = Country]

#check ids
dt_income[, .(country_id = unique(country_id)), by = Country]

#keep balanced sample 
dt_income <- dt_income[!is.na(y)]

#check balance
dt_income[, .(
  min_year = min(year),
  max_year = max(year),
  n_years = uniqueN(year)
), by = Country]

#treated country
treated_country <- "Brazil"

treated_id <- dt_income[
  Country == treated_country,
  unique(country_id)
]

treated_id

#control countries
control_ids <- dt_income[
  Country != treated_country,
  unique(country_id)
]

control_ids

# 6 - prepare data for model ---------------------------------------------------

dataprep.out <- dataprep(
  
  foo = as.data.frame(dt_income),
  
  predictors = c(
    "gini_post_tax"
  ),
  
  predictors.op = "mean",
  
  dependent = "y",
  
  unit.variable = "country_id",
  
  unit.names.variable = "Country",
  
  time.variable = "year",
  
  treatment.identifier = treated_id,
  
  controls.identifier = control_ids,
  
  time.predictors.prior = 1980:1995,
  
  time.optimize.ssr = 1980:1995,
  
  time.plot = 1980:2023
)

# 7 - Run synthetic control ----------------------------------------------------

synth.out <- synth(dataprep.out)

# 8 - Predictor balance --------------------------------------------------------

synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)

# 10 - Path plot ---------------------------------------------------------------

path.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  Ylab = "gini_post_tax",
  Xlab = "Year",
  Main = "Brazil vs Synthetic Brazil"
)

abline(v = 1996, lty = 2)


# 11 - Gap plot ----------------------------------------------------------------

gaps.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  Ylab = "Gap in gini_post_tax",
  Xlab = "Year",
  Main = "Gap between Brazil and Synthetic Brazil"
)

abline(v = 1996, lty = 2)


dt_income[
  Country == "Brazil",
  .(year, pt_share_top0_5)
]

colnames(dt_income)
