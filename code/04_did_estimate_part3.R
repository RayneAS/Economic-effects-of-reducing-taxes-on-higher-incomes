rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "knitr",
  "kableExtra",
  "did",
  "ggplot2",
  "fixest",
  "contdid"
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
library(knitr)
library(kableExtra)
library(did)
library(ggplot2)
library(fixest)
library(contdid)


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  results_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/resultados/part_1"
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output")


# 1 - open data ----------------------------------------------------------------
panel <- data.table(
  read.csv(
    file.path(data_dir,"complete_dataset.csv" )
  )
)

colnames(panel)

#Add filter because inequality data stats at 1980
#(before that there is a lot of missing values)
panel[, year := as.integer(year)]
stopifnot(is.integer(panel$year))

panel <- panel[year >= 1980]

panel[is.na(Reform.Dummy), Reform.Dummy := 0L]
panel[is.na(Significant.Reform), Significant.Reform := 0L]
stopifnot("Reform.Dummy" %in% names(panel))

# 2 - Define auxiliary vars to did package -------------------------------------

setorder(panel, Code, year)

#Define First of treatment (Tax reform)
panel[, first_treat_year :=
        if (any(Reform.Dummy == 1, na.rm=TRUE))
          min(year[Reform.Dummy == 1], na.rm=TRUE)
      else NA_integer_,
      by = Code]

#Define country numeric id did package
panel[, id := .GRP, by = Code]

#Define gvar did package
panel[, gvar := first_treat_year]
panel[is.na(gvar), gvar := 0]

#Define dose as level at adoption
dose_dt <- panel[gvar > 0 & year == gvar, .(dose = -Omega), by = Code]
panel <- merge(panel, dose_dt, by = "Code", all.x = TRUE)

#Define dose=0 for never-treated
panel[gvar == 0, dose := 0]

#dose=0 for treated with Omega missing at adoption (Rubolino-imputed NA)
panel[gvar > 0 & is.na(dose), dose := 0]

#Define continuous adoption: treated only if dose>0
panel[, gvar_cont := fifelse(dose > 0, gvar, 0L)]

panel[gvar_cont == 0, dose := 0]

# 2.1. checks--------------------------------------------------------------------

#check1
panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[gvar > 0]),
  n_never = uniqueN(id[gvar == 0])
)]

panel[, uniqueN(gvar)]
unique_g_var <- sort(unique(panel$gvar))
unique_g_var

#check2
check_gvar <- panel[gvar > 0,
                    .(
                      gvar_unique = unique(gvar),
                      min_year_treated = min(year[Reform.Dummy == 1], na.rm = TRUE)
                    ),
                    by = .(Code, Country)
]

check_gvar[gvar_unique != min_year_treated]

#check3
# sanity checks required by contdid (time-invariant)
stopifnot(panel[, all(uniqueN(dose) == 1L), by = Code]$V1 |> all())
stopifnot(panel[, all(uniqueN(gvar_cont) == 1L), by = Code]$V1 |> all())

#check4
#who switches to never-treated in continuous definition
panel[gvar > 0 & gvar_cont == 0,
      .(Country=unique(Country), gvar=unique(gvar), dose=unique(dose)),
      by=Code][]

#check5
#dose constant by country
panel[, .(uniq_dose = uniqueN(dose)), by = Code][uniq_dose != 1]

#check6
#gvar constant by country
panel[, .(uniq_gvar = uniqueN(gvar)), by = Code][uniq_gvar != 1]

#never-treated: gvar==0 e dose time-invariant (0 ou NA, mas constante)
panel[gvar == 0, .(uniq_dose = uniqueN(dose)), by = Code][uniq_dose != 1]


panel[gvar > 0 & is.na(dose), .N, by = .(Code, Country)][order(-N)]


#check7
#View(panel[,list(Country,Code, year, Reform.Dummy, gvar, dose, Omega)])

panel[gvar > 0 & is.na(dose), .N]
panel[gvar == 0 & is.na(dose), .N]

summary(panel$dose)
table(is.na(panel$dose))
table(is.na(panel$pt_share_top1))
table(is.na(panel$log_gdp_pc))
table(is.na(panel$trade_frac))
table(is.na(panel$gross_fixed_capital_frac))
table(is.na(panel$working_age_pop))
table(is.na(panel$tax_revenue_frac))
table(is.na(panel$gross_savings_frac))


# 3 - TWFE event-study with continuous treatment intensity controls ------------------

#Treatment is omega intensity 

# 3.1. Income share pre tax------------------------------------------------------

#part2: Treatment is omega intensity and we estimate with TWFE

panel[gvar > 0, e := year - gvar]
panel[gvar == 0, e := 0L] # never-treated não tem event time

#selected leds and lags
dt_es <- panel[(gvar == 0) | (e >= -5 & e <= 10)]

nrow(dt_es[complete.cases(
  pt_share_top1,
  dose,
  log_gdp_pc,
  trade_frac,
  gross_fixed_capital_frac,
  gross_savings_frac,
  working_age_pop
)])

panel[gvar > 0 & is.na(dose), .N]



#ESTIMATION
# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m0 <- feols(
  pt_share_top1 ~ i(e, dose, ref = -1) +
    log_gdp_pc + trade_frac + gross_fixed_capital_frac + gross_savings_frac + 
    working_age_pop |
    Code + year,
  data = dt_es, cluster = "Code"
)


summary(m0)

#plot
iplot(m0)

png(file.path(figure_dir, "event_study_dose_share_income1_pretax_TWFE.png"),
    width = 1600, height = 1000, res = 200)

iplot(m0, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()


#latex table
tab_twfe <- etable(
  m0,
  tex = TRUE,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + rmse + ar2 + war2,
  dict = c(
    "e::-5:dose" = "Event time -5 $\\times$ dose",
    "e::-4:dose" = "Event time -4 $\\times$ dose",
    "e::-3:dose" = "Event time -3 $\\times$ dose",
    "e::-2:dose" = "Event time -2 $\\times$ dose",
    "e::0:dose"  = "Event time 0 $\\times$ dose",
    "e::1:dose"  = "Event time 1 $\\times$ dose",
    "e::2:dose"  = "Event time 2 $\\times$ dose",
    "e::3:dose"  = "Event time 3 $\\times$ dose",
    "e::4:dose"  = "Event time 4 $\\times$ dose",
    "e::5:dose"  = "Event time 5 $\\times$ dose",
    "e::6:dose"  = "Event time 6 $\\times$ dose",
    "e::7:dose"  = "Event time 7 $\\times$ dose",
    "e::8:dose"  = "Event time 8 $\\times$ dose",
    "e::9:dose"  = "Event time 9 $\\times$ dose",
    "e::10:dose" = "Event time 10 $\\times$ dose",
    "log_gdp_pc" = "Log GDP per capita",
    "trade_frac" = "Trade openness",
    "gross_fixed_capital_frac" = "Gross fixed capital formation",
    "gross_savings_frac" = "Gross savings",
    "working_age_pop" = "Working-age population"
  ),
  drop = "Intercept",
  title = "TWFE event-study with continuous treatment intensity: Top 1\\% income share (pre-tax)",
  label = "tab:twfe_continuous_pretax",
  notes = c(
    "The dependent variable is the pre-tax top 1\\% income share.",
    "The omitted event time is $e=-1$.",
    "All specifications include country and year fixed effects.",
    "Standard errors clustered at the country level."
  )
)

tab_twfe

#cat(tab_twfe, file = file.path(figure_dir, "tab_twfe_continuous_pretax.tex"))


# 3.2. Income share post tax------------------------------------------------------

#ESTIMATION
# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m0 <- feols(
  d_share_top1 ~ i(e, dose, ref = -1) +
    log_gdp_pc + trade_frac + gross_fixed_capital_frac + gross_savings_frac + 
    working_age_pop |
    Code + year,
  data = dt_es, cluster = "Code"
)


summary(m0)

#plot
iplot(m0)

png(file.path(figure_dir, "event_study_dose_share_income1_post_TWFE.png"),
    width = 1600, height = 1000, res = 200)

iplot(m0, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()


#latex table
tab_twfe <- etable(
  m0,
  tex = TRUE,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + rmse + ar2 + war2,
  dict = c(
    "e::-5:dose" = "Event time -5 $\\times$ dose",
    "e::-4:dose" = "Event time -4 $\\times$ dose",
    "e::-3:dose" = "Event time -3 $\\times$ dose",
    "e::-2:dose" = "Event time -2 $\\times$ dose",
    "e::0:dose"  = "Event time 0 $\\times$ dose",
    "e::1:dose"  = "Event time 1 $\\times$ dose",
    "e::2:dose"  = "Event time 2 $\\times$ dose",
    "e::3:dose"  = "Event time 3 $\\times$ dose",
    "e::4:dose"  = "Event time 4 $\\times$ dose",
    "e::5:dose"  = "Event time 5 $\\times$ dose",
    "e::6:dose"  = "Event time 6 $\\times$ dose",
    "e::7:dose"  = "Event time 7 $\\times$ dose",
    "e::8:dose"  = "Event time 8 $\\times$ dose",
    "e::9:dose"  = "Event time 9 $\\times$ dose",
    "e::10:dose" = "Event time 10 $\\times$ dose",
    "log_gdp_pc" = "Log GDP per capita",
    "trade_frac" = "Trade openness",
    "gross_fixed_capital_frac" = "Gross fixed capital formation",
    "gross_savings_frac" = "Gross savings",
    "working_age_pop" = "Working-age population"
  ),
  drop = "Intercept",
  title = "TWFE event-study with continuous treatment intensity: Top 1\\% income share (post-tax)",
  label = "tab:twfe_continuous_pretax",
  notes = c(
    "The dependent variable is the pre-tax top 1\\% income share.",
    "The omitted event time is $e=-1$.",
    "All specifications include country and year fixed effects.",
    "Standard errors clustered at the country level."
  )
)

tab_twfe

#cat(tab_twfe, file = file.path(figure_dir, "tab_twfe_continuous_pretax.tex"))


# 3.3. Gini pre tax------------------------------------------------------

#ESTIMATION
# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m0 <- feols(
  gini_pre_tax ~ i(e, dose, ref = -1) +
    log_gdp_pc + trade_frac + gross_fixed_capital_frac + gross_savings_frac + 
    working_age_pop |
    Code + year,
  data = dt_es, cluster = "Code"
)


summary(m0)

#plot
iplot(m0)

png(file.path(figure_dir, "event_study_dose_gini_pretax_TWFE.png"),
    width = 1600, height = 1000, res = 200)

iplot(m0, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()

summary(dt_es$dose)

#latex table
tab_twfe <- etable(
  m0,
  tex = TRUE,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + rmse + ar2 + war2,
  dict = c(
    "e::-5:dose" = "Event time -5 $\\times$ dose",
    "e::-4:dose" = "Event time -4 $\\times$ dose",
    "e::-3:dose" = "Event time -3 $\\times$ dose",
    "e::-2:dose" = "Event time -2 $\\times$ dose",
    "e::0:dose"  = "Event time 0 $\\times$ dose",
    "e::1:dose"  = "Event time 1 $\\times$ dose",
    "e::2:dose"  = "Event time 2 $\\times$ dose",
    "e::3:dose"  = "Event time 3 $\\times$ dose",
    "e::4:dose"  = "Event time 4 $\\times$ dose",
    "e::5:dose"  = "Event time 5 $\\times$ dose",
    "e::6:dose"  = "Event time 6 $\\times$ dose",
    "e::7:dose"  = "Event time 7 $\\times$ dose",
    "e::8:dose"  = "Event time 8 $\\times$ dose",
    "e::9:dose"  = "Event time 9 $\\times$ dose",
    "e::10:dose" = "Event time 10 $\\times$ dose",
    "log_gdp_pc" = "Log GDP per capita",
    "trade_frac" = "Trade openness",
    "gross_fixed_capital_frac" = "Gross fixed capital formation",
    "gross_savings_frac" = "Gross savings",
    "working_age_pop" = "Working-age population"
  ),
  drop = "Intercept",
  title = "TWFE event-study with continuous treatment intensity: Top 1\\% income share (post-tax)",
  label = "tab:twfe_continuous_pretax",
  notes = c(
    "The dependent variable is the pre-tax top 1\\% income share.",
    "The omitted event time is $e=-1$.",
    "All specifications include country and year fixed effects.",
    "Standard errors clustered at the country level."
  )
)

tab_twfe

#cat(tab_twfe, file = file.path(figure_dir, "tab_twfe_continuous_pretax.tex"))


# 3.4. Gini post tax------------------------------------------------------

#ESTIMATION
# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m0 <- feols(
  gini_post_tax ~ i(e, dose, ref = -1) +
    log_gdp_pc + trade_frac + gross_fixed_capital_frac + gross_savings_frac + 
    working_age_pop |
    Code + year,
  data = dt_es, cluster = "Code"
)


summary(m0)
summary(dt_es$dose)

#plot
iplot(m0)

png(file.path(figure_dir, "event_study_dose_gini_posttax_TWFE.png"),
    width = 1600, height = 1000, res = 200)

iplot(m0, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()


summary(dt_es$dose)

#latex table
tab_twfe <- etable(
  m0,
  tex = TRUE,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + rmse + ar2 + war2,
  dict = c(
    "e::-5:dose" = "Event time -5 $\\times$ dose",
    "e::-4:dose" = "Event time -4 $\\times$ dose",
    "e::-3:dose" = "Event time -3 $\\times$ dose",
    "e::-2:dose" = "Event time -2 $\\times$ dose",
    "e::0:dose"  = "Event time 0 $\\times$ dose",
    "e::1:dose"  = "Event time 1 $\\times$ dose",
    "e::2:dose"  = "Event time 2 $\\times$ dose",
    "e::3:dose"  = "Event time 3 $\\times$ dose",
    "e::4:dose"  = "Event time 4 $\\times$ dose",
    "e::5:dose"  = "Event time 5 $\\times$ dose",
    "e::6:dose"  = "Event time 6 $\\times$ dose",
    "e::7:dose"  = "Event time 7 $\\times$ dose",
    "e::8:dose"  = "Event time 8 $\\times$ dose",
    "e::9:dose"  = "Event time 9 $\\times$ dose",
    "e::10:dose" = "Event time 10 $\\times$ dose",
    "log_gdp_pc" = "Log GDP per capita",
    "trade_frac" = "Trade openness",
    "gross_fixed_capital_frac" = "Gross fixed capital formation",
    "gross_savings_frac" = "Gross savings",
    "working_age_pop" = "Working-age population"
  ),
  drop = "Intercept",
  title = "TWFE event-study with continuous treatment intensity: Top 1\\% income share (post-tax)",
  label = "tab:twfe_continuous_pretax",
  notes = c(
    "The dependent variable is the pre-tax top 1\\% income share.",
    "The omitted event time is $e=-1$.",
    "All specifications include country and year fixed effects.",
    "Standard errors clustered at the country level."
  )
)

tab_twfe

#cat(tab_twfe, file = file.path(figure_dir, "tab_twfe_continuous_pretax.tex"))
