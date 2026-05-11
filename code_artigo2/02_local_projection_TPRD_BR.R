rm(list = ls())
gc()

# =============================================================================
# Local Projections - Tax reforms and inequality
# Brazil-only exploratory exercise
# =============================================================================

# 0 - Packages ----------------------------------------------------------------

packages <- c(
  "data.table",
  "readr",
  "readxl",
  "haven",
  "ggplot2",
  "scales",
  "countrycode",
  "fixest"
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

# 1 - Directories -------------------------------------------------------------

user <- "Rayne"

if (user == "Rayne") {
  
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/TPRD"
  
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output_artigo2")


# 2 - Open TPRD data ----------------------------------------------------------

raw_dt <- as.data.table(read_excel(
  file.path(data_dir2, "taxmeasuresdatabase.xlsx"),
  sheet = "TPRD"
))

dt_fig <- copy(raw_dt)

dt_fig[, year_announcement := as.numeric(year_announcement)]


# 3 - Initial checks ----------------------------------------------------------

table(dt_fig$year_announcement)
table(dt_fig$TAX_major)
table(dt_fig$TAX_change)
table(dt_fig$TAX_type)
table(dt_fig$TAX_reformtype)

# Country-year counts by reform type/change
n_base_dec <- nrow(unique(dt_fig[
  TAX_reformtype == "BASE" & TAX_change == "DEC",
  .(country, year_announcement)
]))

n_base_inc <- nrow(unique(dt_fig[
  TAX_reformtype == "BASE" & TAX_change == "INC",
  .(country, year_announcement)
]))

n_rate_dec <- nrow(unique(dt_fig[
  TAX_reformtype == "RATE" & TAX_change == "DEC",
  .(country, year_announcement)
]))

n_rate_inc <- nrow(unique(dt_fig[
  TAX_reformtype == "RATE" & TAX_change == "INC",
  .(country, year_announcement)
]))

n_base <- nrow(unique(dt_fig[
  TAX_reformtype == "BASE",
  .(country, year_announcement)
]))

n_rate <- nrow(unique(dt_fig[
  TAX_reformtype == "RATE",
  .(country, year_announcement)
]))

n_all <- nrow(unique(dt_fig[, .(country, year_announcement)]))
n_measures <- nrow(dt_fig)
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


# 4 - Clean TPRD sample -------------------------------------------------------

dt_fig <- dt_fig[
  year_announcement >= 1990 &
    !country %in% c("CHN", "IND") &
    TAX_major == 1
]

# Direction of reform:
# INC = +1, DEC = -1
dt_fig[, a := fifelse(TAX_change == "INC", 1L, -1L)]

table(dt_fig$TAX_change)
table(dt_fig$a)


# 5 - Build reform-shock panel ------------------------------------------------

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

year_min <- 1990
year_max <- 2014

countries <- sort(unique(est_wide$country))

skeleton <- CJ(
  country = countries,
  year = seq(year_min, year_max)
)

est_wide_full <- merge(
  skeleton,
  est_wide,
  by = c("country", "year"),
  all.x = TRUE
)

shock_vars <- setdiff(names(est_wide), c("country", "year"))

for (v in shock_vars) {
  set(
    est_wide_full,
    i = which(is.na(est_wide_full[[v]])),
    j = v,
    value = 0
  )
}


# 6 - Open inequality data ----------------------------------------------------

dt_income <- as.data.table(read_csv(
  file.path(data_dir, "final_data_inequality_WID.csv")
))

dt_income[, year := as.integer(year)]
setnames(dt_income, "Code", "country")

# Harmonize country codes
dt_income[, country_iso3 := countrycode(country, "iso2c", "iso3c")]
dt_income[country_iso3 == "DEU", country_iso3 := "GER"]
dt_income[, country := country_iso3]
dt_income[, country_iso3 := NULL]


# 7 - Open and clean controls -------------------------------------------------

dt_controls <- as.data.table(read_csv(
  file.path(data_dir, "control_variables_all_countries.csv")
))

dt_controls[, year := as.integer(year)]

# Harmonize Germany code
dt_controls[Code == "DEU", Code := "GER"]

setnames(dt_controls, c("Code", "Country"), c("country", "country_name"))

# Keep only countries in estimation sample
dt_controls <- dt_controls[
  country %in% unique(est_wide_full$country)
]

# Check duplicates before merge
dt_controls[, .N, by = .(country, year)][N > 1]

# Keep selected controls
dt_controls <- dt_controls[, .(
  country,
  country_name,
  year,
  gdp_pc,
  trade,
  working_age_pop
)]


# 8 - Merge datasets ----------------------------------------------------------

panel_data <- merge(
  est_wide_full,
  dt_income,
  by = c("country", "year"),
  all.x = TRUE
)

panel_data <- merge(
  panel_data,
  dt_controls,
  by = c("country", "year"),
  all.x = TRUE
)

setorder(panel_data, country, year)


# 9 - Critical checks ---------------------------------------------------------

# Duplicates
panel_data[, .N, by = .(country, year)][N > 1]

# Missing shocks
panel_data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = shock_vars]

# Number of years by country
panel_data[, .N, by = country]

# Missing outcome
panel_data[, sum(is.na(d_share_top0_01))]

# Missing controls for Brazil
panel_data[country == "BRA",
           .(
             missing_gdp_pc = sum(is.na(gdp_pc)),
             missing_trade = sum(is.na(trade)),
             missing_working_age_pop = sum(is.na(working_age_pop))
           )]


# 10 - Transform controls -----------------------------------------------------

panel_data[, log_gdp_pc := log(gdp_pc)]
panel_data[, trade_frac := trade / 100]
panel_data[, working_age_pop_frac := working_age_pop / 100]


# 11 - Create leads -----------------------------------------------------------

for (h in 0:5) {
  panel_data[, paste0("y_h", h) :=
               shift(d_share_top0_01, n = h, type = "lead"),
             by = country]
}


# 12 - Brazil sample ----------------------------------------------------------

panel_br <- copy(panel_data[country == "BRA"])
setorder(panel_br, year)

# PIT shock dummy
panel_br[, shock := as.integer((PIT_b != 0) | (PIT_r != 0))]

# Alternative shocks
# panel_br[, shock := as.integer(PIT_b != 0)]
# panel_br[, shock := as.integer(PIT_r != 0)]

# Trend
panel_br[, trend := year - min(year, na.rm = TRUE)]

# Outcome lags
panel_br[, y_lag1 := shift(d_share_top0_01, 1)]
panel_br[, y_lag2 := shift(d_share_top0_01, 2)]

# Cumulative dependent variable: y_{t+h} - y_{t-1}
for (h in 0:5) {
  panel_br[, paste0("dep_h", h) :=
             shift(d_share_top0_01, n = h, type = "lead") - y_lag1]
}

# Check shock years
panel_br[shock == 1, .(year, PIT_b, PIT_r, shock)]


# 13 - Estimate LPs -----------------------------------------------------------

horizons <- 0:5

specs <- list(
  baseline = "shock + y_lag1 + y_lag2 + trend",
  macro = "shock + y_lag1 + y_lag2 + trend + log_gdp_pc + trade_frac",
  demo = "shock + y_lag1 + y_lag2 + trend + working_age_pop_frac",
  full = "shock + y_lag1 + y_lag2 + trend + log_gdp_pc + trade_frac + working_age_pop_frac"
)

lp_results <- list()

for (s in names(specs)) {
  
  lp_results[[s]] <- lapply(horizons, function(h) {
    
    dep_var <- paste0("dep_h", h)
    
    feols(
      as.formula(paste0(dep_var, " ~ ", specs[[s]])),
      data = panel_br
    )
  })
}


# 14 - Extract IRFs -----------------------------------------------------------

irf_all <- rbindlist(lapply(names(lp_results), function(s) {
  
  data.table(
    spec = s,
    h = horizons,
    beta = sapply(lp_results[[s]], function(x) coef(x)["shock"]),
    se = sapply(lp_results[[s]], function(x) se(x)["shock"])
  )
}))

irf_all[, upper := beta + 1.96 * se]
irf_all[, lower := beta - 1.96 * se]


# 15 - Plot IRFs --------------------------------------------------------------

fig1 <- ggplot(irf_all, aes(x = h, y = beta, color = spec, group = spec)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Local projections com diferentes controles - Brasil",
    x = "Horizonte em anos",
    y = expression(y[t+h] - y[t-1]),
    color = "Especificação"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "LP_com_diferentes_controles_Brasil.jpg"),
  plot = fig1,
  height = 4,
  width = 6
)



fig2 <- ggplot(irf_all, aes(x = h, y = beta)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ spec) +
  labs(
    title = "Local projections por especificação - Brasil",
    x = "Horizonte em anos",
    y = expression(y[t+h] - y[t-1])
  ) +
  theme_minimal()


ggsave(
  file.path(figure_dir, "LP_por_especificação_Brasil.jpg"),
  plot = fig2,
  height = 4,
  width = 6
)

# 16 - Plot shock years -------------------------------------------------------

fig <-ggplot(panel_br, aes(year, shock)) +
  geom_col() +
  labs(
    title = "Anos com choque PIT - Brasil",
    x = "Ano",
    y = "Choque"
  )

ggsave(
  file.path(figure_dir, "anos_pit_choque_Brasil.jpg"),
  plot = fig,
  height = 4,
  width = 6
)


# 17 - Placebo shock ----------------------------------------------------------

#Define the placebo as a shock 5 years after the real schock

panel_br[, fake_shock := shift(shock, 5)]
panel_br[fake_shock == 1, .(year, shock, fake_shock)]


placebo_results <- lapply(horizons, function(h) {
  
  dep_var <- paste0("dep_h", h)
  
  feols(
    as.formula(
      paste0(dep_var,
             " ~ fake_shock + y_lag1 + y_lag2 + trend")
    ),
    data = panel_br
  )
})


irf_placebo <- data.table(
  h = horizons,
  beta = sapply(placebo_results,
                function(x) coef(x)["fake_shock"]),
  se = sapply(placebo_results,
              function(x) se(x)["fake_shock"])
)

irf_placebo[, upper := beta + 1.96 * se]
irf_placebo[, lower := beta - 1.96 * se]

irf_placebo[, spec := "placebo"]

irf_comp <- irf_all[ spec == "baseline"]

irf_compare <- rbind(
  irf_comp[, .(h, beta, lower, upper, spec)],
  irf_placebo[, .(h, beta, lower, upper, spec)]
)


fig3 <-ggplot(irf_compare,
       aes(x = h,
           y = beta,
           color = spec,
           group = spec)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  
  labs(
    title = "Baseline vs placebo shock",
    x = "Horizonte em anos",
    y = expression(y[t+h] - y[t-1]),
    color = "Modelo"
  ) +
  
  theme_minimal()


ggsave(
  file.path(figure_dir, "LP_por_placebo_baseline_Brasil.jpg"),
  plot = fig3,
  height = 4,
  width = 6
)

# 18 - Panel LP baseline -------------------------------------------------------

panel_lp <- copy(panel_data)
setorder(panel_lp, country, year)

# PIT shock dummy
panel_lp[, shock := as.integer((PIT_b != 0) | (PIT_r != 0))]

# Outcome lags
panel_lp[, y_lag1 := shift(d_share_top0_01, 1), by = country]
panel_lp[, y_lag2 := shift(d_share_top0_01, 2), by = country]

# Cumulative dependent variable: y_{i,t+h} - y_{i,t-1}
for (h in 0:5) {
  panel_lp[, paste0("dep_h", h) :=
             shift(d_share_top0_01, n = h, type = "lead") - y_lag1,
           by = country]
}

# Check shock distribution
panel_lp[shock == 1, .N, by = country][order(-N)]
panel_lp[shock == 1, .N, by = year][order(year)]

# Baseline panel LP
horizons <- 0:5

panel_results <- lapply(horizons, function(h) {
  
  dep_var <- paste0("dep_h", h)
  
  feols(
    as.formula(
      paste0(dep_var, " ~ shock + y_lag1 + y_lag2 | country + year")
    ),
    data = panel_lp,
    cluster = ~ country
  )
})

irf_panel <- data.table(
  h = horizons,
  beta = sapply(panel_results, function(x) coef(x)["shock"]),
  se = sapply(panel_results, function(x) se(x)["shock"]),
  nobs = sapply(panel_results, nobs)
)

irf_panel[, upper := beta + 1.96 * se]
irf_panel[, lower := beta - 1.96 * se]

irf_panel


fig_panel_baseline <- ggplot(irf_panel, aes(x = h, y = beta)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Panel local projections - PIT reforms",
    x = "Horizon in years",
    y = expression(y[i*t+h] - y[i*t-1])
  ) +
  theme_minimal()

fig_panel_baseline

ggsave(
  file.path(figure_dir, "LP_panel_baseline_PIT.jpg"),
  plot = fig_panel_baseline,
  height = 4,
  width = 6
)

fwrite(
  irf_panel,
  file.path(figure_dir, "irf_panel_baseline_PIT.csv")
)



fig5 <-ggplot(panel_lp, aes(year, shock)) +
  geom_col() +
  labs(
    title = "Anos com choque PIT - Painel de Países",
    x = "Ano",
    y = "Choque"
  )

ggsave(
  file.path(figure_dir, "anos_pit_choque_painel.jpg"),
  plot = fig5,
  height = 4,
  width = 6
)
