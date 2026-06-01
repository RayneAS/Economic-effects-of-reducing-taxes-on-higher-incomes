#obs:Inequality data it was cleaned and organized by Mariana in another code 

rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "haven",
  "Synth",
  "ggplot2",
  "xtable"
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

  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output2")
figure_dir

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

outcome_var <- "d_share_top1"
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


# 5 - Plot inequality measures ------------------------------------------------

# Variables to plot
ineq_vars <- c(
  "d_share_p0_10",
  "d_share_p90_100",
  "d_share_p95_100",
  "d_share_top1",
  "d_share_top0_5",
  "d_share_top0_1",
  "d_share_top0_01",
  "gini_post_tax"
)


# Loop over variables
for (var in ineq_vars) {
  
  p <- ggplot(
    dt_income,
    aes(
      x = year,
      y = get(var),
      color = Country
    )
  ) +
    geom_line(size = 1) +
    
    geom_vline(
      xintercept = 1996,
      linetype = "dashed"
    ) +
    
    labs(
      title = "",
      x = "Year",
      y = var,
      color = "Country"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  print(p)
  
  ggsave(
    filename = paste0(var, ".png"),
    plot = p,
    path = figure_dir,
    width = 10,
    height = 6
  )
}

# 6 - organize data to run SCM--------------------------------------------------

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

# 7 - prepare data for model ---------------------------------------------------

dataprep.out <- dataprep(
  
  foo = as.data.frame(dt_income),
  
  predictors = c(
    "d_share_top1",
    "d_share_top0_5",
    "d_share_top0_1",
    "d_share_p90_100",
    "d_share_p95_100",
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

# 8 - Run synthetic control ----------------------------------------------------

synth.out <- synth(dataprep.out)

# 9 - Predictor balance --------------------------------------------------------

synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)


# 9.1 - Synthetic control weights

tab_weights <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)$tab.w

tab_weights <- data.table(tab_weights)

tab_weights

#rename columns
setnames(
  tab_weights,
  old = c("w.weights", "unit.names"),
  new = c("Weight", "Country")
)

#keep relevant columns
tab_weights_export <- tab_weights[, .(Country, Weight)]

#create latex table
weights_tex <- xtable(
  tab_weights_export,
  caption = "Synthetic control weights for Brazil",
  label = "tab:weights"
)

print(
  weights_tex,
  file = file.path(figure_dir, "synthetic_weights.tex"),
  include.rownames = FALSE
)


# 9.2 - Predictor balance

tab_balance <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)$tab.pred

tab_balance <- data.table(tab_balance)

tab_balance[, predictor := c(
  "Top 1% disposable income share",
  "Top 0.5% disposable income share",
  "Top 0.1% disposable income share",
  "Top 10% disposable income share",
  "Top 5% disposable income share",
  "Disposable income Gini"
)]

setcolorder(tab_balance, c("predictor", "Treated", "Synthetic", "Sample Mean"))

tab_balance

#round values
tab_balance_export <- copy(tab_balance)

tab_balance_export[, c("Treated", "Synthetic", "Sample Mean") :=
                     lapply(.SD, round, 3),
                   .SDcols = c("Treated", "Synthetic", "Sample Mean")]

#create latex table
balance_tex <- xtable(
  tab_balance_export,
  caption = "Predictor balance before treatment",
  label = "tab:balance"
)

print(
  balance_tex,
  file = file.path(figure_dir, "predictor_balance.tex"),
  include.rownames = FALSE
)


# 10 - Path plot ---------------------------------------------------------------

png(
  filename = file.path(figure_dir, "path_plot_top1.png"),
  width = 1200,
  height = 800,
  res = 150
)

path.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  Ylab = "Top 1% disposable income share",
  Xlab = "Year",
  Main = "Brazil vs Synthetic Brazil"
)

abline(v = 1996, lty = 2)

dev.off()

# 11 - Gap plot ----------------------------------------------------------------

png(
  filename = file.path(figure_dir, "gap_plot_top1.png"),
  width = 1200,
  height = 800,
  res = 150
)

gaps.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  Ylab = "Gap in top 1% disposable income share",
  Xlab = "Year",
  Main = "Gap between Brazil and Synthetic Brazil"
)

abline(v = 1996, lty = 2)

dev.off()

# 12 - Estimate average post-treatment effect ---------------------------------

# Observed Brazil
Y_treated <- dataprep.out$Y1plot

# Synthetic Brazil
Y_synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Years
years <- dataprep.out$tag$time.plot

# Organize results
scm_effects <- data.table(
  year = years,
  treated = as.numeric(Y_treated),
  synthetic = as.numeric(Y_synth)
)

scm_effects[, gap := treated - synthetic]
scm_effects[, post := year >= treat_year]

# Average post-treatment gap
avg_gap_post <- scm_effects[post == TRUE, mean(gap, na.rm = TRUE)]

# Baseline level: Brazil in 1995
baseline_1995 <- scm_effects[year == 1995, treated]

# Effect as % of 1995 baseline
avg_gap_pct_1995 <- 100 * avg_gap_post / baseline_1995

# Alternative baseline: average Brazil pre-treatment level
baseline_pre_mean <- scm_effects[year < treat_year, mean(treated, na.rm = TRUE)]

avg_gap_pct_pre_mean <- 100 * avg_gap_post / baseline_pre_mean

# Print summary
summary_effect <- data.table(
  outcome = outcome_var,
  avg_post_gap = avg_gap_post,
  baseline_1995 = baseline_1995,
  avg_gap_pct_1995 = avg_gap_pct_1995,
  baseline_pre_mean = baseline_pre_mean,
  avg_gap_pct_pre_mean = avg_gap_pct_pre_mean
)

summary_effect

# 13 - In-time placebo --------------------------------------------------------

fake_treat_year <- 1990

dataprep.placebo.time <- dataprep(
  foo = as.data.frame(dt_income),
  
  predictors = c(
    "d_share_top1",
    "d_share_top0_5",
    "d_share_top0_1",
    "d_share_p90_100",
    "d_share_p95_100",
    "gini_post_tax"
  ),
  
  predictors.op = "mean",
  dependent = "y",
  
  unit.variable = "country_id",
  unit.names.variable = "Country",
  time.variable = "year",
  
  treatment.identifier = treated_id,
  controls.identifier = control_ids,
  
  time.predictors.prior = 1980:(fake_treat_year - 1),
  time.optimize.ssr = 1980:(fake_treat_year - 1),
  time.plot = 1980:1995
)

synth.placebo.time <- synth(dataprep.placebo.time)

png(
  filename = file.path(figure_dir, "placebo_time_1990_top1.png"),
  width = 1200,
  height = 800,
  res = 150
)

gaps.plot(
  synth.res = synth.placebo.time,
  dataprep.res = dataprep.placebo.time,
  Ylab = "Gap in top 1% disposable income share",
  Xlab = "Year",
  Main = "In-time placebo: fake treatment in 1990"
)

abline(v = fake_treat_year, lty = 2)

dev.off()

# 14 - In-space placebo -------------------------------------------------------

placebo_results <- list()

all_units <- dt_income[, unique(country_id)]

for (placebo_id in all_units) {
  
  placebo_country <- dt_income[
    country_id == placebo_id,
    unique(Country)
  ]
  
  placebo_controls <- all_units[all_units != placebo_id]
  
  dataprep.placebo <- dataprep(
    foo = as.data.frame(dt_income),
    
    predictors = c(
      "d_share_top1",
      "d_share_top0_5",
      "d_share_top0_1",
      "d_share_p90_100",
      "d_share_p95_100",
      "gini_post_tax"
    ),
    
    predictors.op = "mean",
    dependent = "y",
    
    unit.variable = "country_id",
    unit.names.variable = "Country",
    time.variable = "year",
    
    treatment.identifier = placebo_id,
    controls.identifier = placebo_controls,
    
    time.predictors.prior = 1980:1995,
    time.optimize.ssr = 1980:1995,
    time.plot = 1980:2023
  )
  
  synth.placebo <- synth(dataprep.placebo)
  
  Y_treated_placebo <- dataprep.placebo$Y1plot
  Y_synth_placebo <- dataprep.placebo$Y0plot %*% synth.placebo$solution.w
  years_placebo <- dataprep.placebo$tag$time.plot
  
  placebo_results[[placebo_country]] <- data.table(
    Country = placebo_country,
    year = years_placebo,
    treated = as.numeric(Y_treated_placebo),
    synthetic = as.numeric(Y_synth_placebo),
    gap = as.numeric(Y_treated_placebo - Y_synth_placebo)
  )
}

dt_placebo <- rbindlist(placebo_results)

p_placebo <- ggplot(
  dt_placebo,
  aes(x = year, y = gap, group = Country)
) +
  geom_line(aes(color = Country == "Brazil"), linewidth = 1) +
  geom_vline(xintercept = 1996, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Gap in top 1% disposable income share",
    title = "In-space placebo tests"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = "placebo_space_top1.png",
  plot = p_placebo,
  path = figure_dir,
  width = 10,
  height = 6
)