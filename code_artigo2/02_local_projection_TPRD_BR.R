rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "haven",
  "ggplot2",
  "scales",
  "countrycode"
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
library(readxl)
library(haven)
library(ggplot2)
library(scales)
library(countrycode)
library(fixest)




# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/TPRD"
  
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output_artigo2")


# 1 - open raw data------------------------------------------------------------

raw_dt <- as.data.table(read_excel(
  file.path(data_dir2, "taxmeasuresdatabase.xlsx"),
  sheet = "TPRD"
))

dt_fig <- copy(raw_dt)

colnames(dt_fig)

class(dt_fig$year_announcement)
dt_fig[, year_announcement := as.numeric(year_announcement)]

table(dt_fig$year_announcement)

table(dt_fig$TAX_major)
table(dt_fig$TAX_change)
table(dt_fig$TAX_type)
table(dt_fig$TAX_reformtype)

#checagens

# 1. BASE
n_base_dec <- nrow(unique(
  dt_fig[
    TAX_reformtype == "BASE" & TAX_change == "DEC",
    .(country, year_announcement)
  ]
))


n_base_inc <- nrow(unique(
  dt_fig[
    TAX_reformtype == "BASE" & TAX_change == "INC",
    .(country, year_announcement)
  ]
))

# 2. RATE
n_rate_dec <- nrow(unique(
  dt_fig[
    TAX_reformtype == "RATE" & TAX_change == "DEC",
    .(country, year_announcement)
  ]
))

n_rate_inc <- nrow(unique(
  dt_fig[
    TAX_reformtype == "RATE" & TAX_change == "INC",
    .(country, year_announcement)
  ]
))

# 3. BASE country-year
n_base <- nrow(unique(
  dt_fig[TAX_reformtype == "BASE", .(country, year_announcement)]
))

# 4. RATE country-year
n_rate <- nrow(unique(
  dt_fig[TAX_reformtype == "RATE", .(country, year_announcement)]
))

# 5. ALL country-year
n_all <- nrow(unique(dt_fig[, .(country, year_announcement)]))

# 6. total measures
n_measures <- nrow(dt_fig)

# 7. average
avg <- n_measures / n_all

list(
  base_country_year = n_base,
  rate_country_year = n_rate,
  all_country_year = n_all,
  measures = n_measures,
  avg = avg
)

list(
  base_dec = n_base_dec,
  base_inc = n_base_inc,
  rate_dec = n_rate_dec,
  rate_inc = n_rate_inc
)

#filter data to be like the original paper
dt_fig <- dt_fig[year_announcement>=1990]
dt_fig <- dt_fig[country!="CHN" & country!="IND"]
dt_fig <- dt_fig[TAX_major==1]


# Code direction of each raw reform:
# INC = +1, DEC = -1
dt_fig[, a := fifelse(TAX_change == "INC", 1L, -1L)]

#View(dt_fig[,list(country,year_announcement, TAX_change, a, TAX_reformtype)]) 

table(dt_fig$TAX_change)
table(dt_fig$a)


# 3 - Build estimation panel --------------------------------------------------

# starting from dt_fig after:
# year >= 1990, exclude CHN/IND, TAX_major == 1,
# TAX_reformtype in BASE/RATE, TAX_change in INC/DEC, TAX_type in six taxes

est_dt <- dt_fig[
  ,
  .(TC = sum(a)),
  by = .(country, year_announcement, TAX_type, TAX_reformtype)
]

est_dt[, shock_var := paste0(
  TAX_type, "_",
  ifelse(TAX_reformtype == "BASE", "b", "r")
)]

est_wide <- dcast(
  est_dt,
  country + year_announcement ~ shock_var,
  value.var = "TC",
  fill = 0
)

setnames(est_wide, "year_announcement", "year")
est_wide[, year := as.integer(year)]

# define estimation window from inequality data or chosen sample
year_min <- 1990
year_max <- 2014

# countries you want in the final sample
countries <- sort(unique(est_wide$country))

# full country-year skeleton
skeleton <- CJ(
  country = countries,
  year = seq(year_min, year_max)
)

# merge reform shocks onto full panel
est_wide_full <- merge(
  skeleton,
  est_wide,
  by = c("country", "year"),
  all.x = TRUE
)

# identify shock columns
shock_vars <- setdiff(names(est_wide), c("country", "year"))

# replace missing shocks with zero
for (v in shock_vars) {
  set(
    est_wide_full,
    i = which(is.na(est_wide_full[[v]])),
    j = v,
    value = 0
  )
}

# inequality data-----------------------------------
dt_income <- data.table(read_csv(file.path(data_dir, 
                                           "final_data_inequality_WID.csv")))
dt_income[, year := as.integer(year)]
setnames(dt_income, "Code", "country")

# if country is iso2 here
dt_income[, country_iso3 := countrycode(country, "iso2c", "iso3c")]
dt_income[country_iso3 == "DEU", country_iso3 := "GER"]
dt_income[, country := country_iso3]
dt_income[, country_iso3 := NULL]

# now merge with inequality data
panel_data <- merge(
  est_wide_full,
  dt_income,
  by = c("country", "year"),
  all.x = TRUE
)

setorder(panel_data, country, year)

# controls data-----------------------------------
dt_controls <- data.table(
  read_csv(
    file.path(data_dir, "control_variables_all_countries.csv")))

setdiff(sort(unique(panel_data$country)), sort(unique(dt_controls$Code)))

dt_controls[, country_iso3 := countrycode(Code, "iso2c", "iso3c")]
dt_controls[country_iso3 == "DEU", country_iso3 := "GER"]
dt_controls[, Code := country_iso3]
dt_controls[, country_iso3 := NULL]

# critical checks
panel_data[, .N, by = .(country, year)][N > 1]

panel_data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = shock_vars]

panel_data[, .N, by = country]

panel_data[, sum(is.na(d_share_top0_01))]

#Create leads
setorder(panel_data, country, year)

for (h in 0:5) {
  panel_data[, paste0("y_h", h) :=
               shift(d_share_top0_01, n = h, type = "lead"),
             by = country
  ]
}

# 4 - Estimate for Brazil -----------------------------------------

panel_br <- copy(panel_data[country == "BRA"])
setorder(panel_br, year)

# Choque PIT
#panel_br[, shock := as.integer((PIT_b != 0) | (PIT_r != 0))]
#panel_br[, shock := as.integer((PIT_b != 0))]
panel_br[, shock := as.integer((PIT_r != 0))]

#tendencia
panel_br[, trend := year - min(year, na.rm = TRUE)]

# Lags do outcome
panel_br[, y_lag1 := shift(d_share_top0_01, 1)]
panel_br[, y_lag2 := shift(d_share_top0_01, 2)]

# Variável dependente acumulada: y_{t+h} - y_{t-1}
for (h in 0:5) {
  panel_br[, paste0("dep_h", h) :=
             shift(d_share_top0_01, n = h, type = "lead") - y_lag1]
}

horizons <- 0:5

results <- lapply(horizons, function(h) {
  
  dep_var <- paste0("dep_h", h)
  
  feols(
    as.formula(paste0(dep_var, " ~ shock + y_lag1 + y_lag2 + trend")),
    data = panel_br
  )
})

irf <- data.table(
  h = horizons,
  beta = sapply(results, function(x) coef(x)["shock"]),
  se = sapply(results, function(x) se(x)["shock"])
)

irf[, upper := beta + 1.96 * se]
irf[, lower := beta - 1.96 * se]


#grafico---------------------------
ggplot(irf, aes(x = h, y = beta)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Resposta da desigualdade a choques tributários - Brasil",
    x = "Horizonte em anos",
    y = expression(y[t+h] - y[t-1])
  )

ggplot(panel_br, aes(year, shock)) +
  geom_col()


panel_br[, fake_shock := shift(shock, 5)]