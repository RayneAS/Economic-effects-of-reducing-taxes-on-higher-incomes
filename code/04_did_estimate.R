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
  "fastglm",
  "ggplot2",
  "fixest"
)




installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))


# install.packages("fastglm", type = "source", 
#                  repos = "https://cran.r-project.org")

#PACKAGES USED
library(data.table)
library(readr)
library(knitr)
library(kableExtra)
library(did)
library(ggplot2)
#library(fastglm)
library(fixest)



# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
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
panel <- panel[year >= 1980]

panel[is.na(Reform.Dummy), Reform.Dummy := 0L]


# 2 - Define auxiliar vars to did package ------------------------------------------------------

setorder(panel, Code, year)

# ever-treated indicator (country-level)
panel[, treated_group := as.integer(any(Reform.Dummy == 1, na.rm = TRUE)), 
      by = Code]

#View(panel[,list(Country, year, Reform.Dummy, treated_group)])
panel[treated_group == 1, uniqueN(Country)]
panel[, .N, by = treated_group]
panel[treated_group == 1, uniqueN(Code)]
#View(panel[,list(Country, year, Reform.Dummy, treated_group)])


#Define First of treatment (Tax reform)
panel[, first_treat_year :=
        if (any(Reform.Dummy == 1, na.rm=TRUE))
          min(year[Reform.Dummy == 1], na.rm=TRUE)
      else NA_integer_,
      by = Code]


panel[Country=="Australia",
      .(first_treat_year=unique(first_treat_year))]

panel[Country=="Japan",
      .(first_treat_year=unique(first_treat_year))]


# Define pre-period flag
panel[, pre_period := 0L]
panel[treated_group == 1 & !is.na(first_treat_year) & 
        year < first_treat_year, pre_period := 1L]
panel[treated_group == 0, pre_period := 1L]


# View(panel[,list(Country, year, Reform.Dummy, pre_period,
#                  treated_group, first_treat_year)])

#Define country numeric id did package
panel[, id := .GRP, by = Code]

#Define gvar did package
panel[, gvar := first_treat_year]
panel[is.na(gvar), gvar := 0]


# View(panel[,list(Country, year, id, Reform.Dummy,
#                  treated_group,first_treat_year, gvar)])

#data checks
panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[gvar > 0]),
  n_never = uniqueN(id[gvar == 0])
)]

# test <- panel[treated_group==1]
# unique_countri <- unique(test$Country)
# unique_countri

panel[, uniqueN(gvar)]
unique_g_var <- sort(unique(panel$gvar))
unique_g_var

#check g_var
check_gvar <- panel[gvar > 0,
                    .(
                      gvar_unique = unique(gvar),
                      min_year_treated = min(year[Reform.Dummy == 1], na.rm = TRUE)
                    ),
                    by = .(Code, Country)
]

check_gvar[gvar_unique != min_year_treated]

# 3 - Baseline (pre-treatment) summary stats at COUNTRY level------------------------------------------

vars_baseline <- c(
  "share_income1",
  "log_gdp_pc",
  "trade_frac",
  "tax_revenue_frac",
  "gross_fixed_capital_frac",
  "working_age_pop"
)

missing_vars <- setdiff(vars_baseline, names(panel))
if (length(missing_vars) > 0) {
  stop("Variáveis ausentes em vars_baseline: ", paste(missing_vars, collapse = ", "))
}


#ever-treated: years < gvar
#never-treated: in all years in the sample
panel_pre <- panel[(gvar == 0L) | (year < gvar)]


yr_min <- panel_pre[treated_group == 1, min(year, na.rm = TRUE)]
yr_max <- panel_pre[treated_group == 1, max(year, na.rm = TRUE)]
panel_pre <- panel_pre[year >= yr_min & year <= yr_max]

#mean by country - for selected vars
country_pre <- panel_pre[, lapply(.SD, function(x) {
  x2 <- x[is.finite(x)]
  if (length(x2) == 0) NA_real_ else mean(x2)
}), by = .(Code, Country, treated_group), .SDcols = vars_baseline]


# function test of mean difference between countries
diff_pval <- function(x_treat, x_ctrl) {
  # remove NA
  xt <- x_treat[is.finite(x_treat)]
  xc <- x_ctrl[is.finite(x_ctrl)]
  if (length(xt) < 2 || length(xc) < 2) return(NA_real_)
  tryCatch(t.test(xt, xc)$p.value, error = function(e) NA_real_)
}


#table: treated vs never treated + diff + p-val
baseline_table <- rbindlist(lapply(vars_baseline, function(v){
  x_treat <- country_pre[treated_group == 1, get(v)]
  x_ctrl  <- country_pre[treated_group == 0, get(v)]
  
  mean_treat <- mean(x_treat, na.rm = TRUE)
  mean_ctrl  <- mean(x_ctrl,  na.rm = TRUE)
  
  data.table(
    Variable     = v,
    N_Treated    = sum(is.finite(x_treat)),
    N_Never      = sum(is.finite(x_ctrl)),
    Treated      = mean_treat,
    Never        = mean_ctrl,
    Diff         = mean_treat - mean_ctrl,
    P_value      = diff_pval(x_treat, x_ctrl)
  )
}), fill = TRUE)


#labels
baseline_table[, Variable := fifelse(Variable %chin% names(var_labels),
                                     var_labels[Variable], Variable)]


#round
baseline_table[, `:=`(
  Treated = round(Treated, 3),
  Never   = round(Never, 3),
  Diff    = round(Diff, 3),
  P_value = round(P_value, 3)
)]


#LaTeX table
kbl(
  baseline_table[, .(Variable, N_Treated, N_Never, Treated, Never, Diff, P_value)],
  format = "latex",
  booktabs = TRUE,
  align = "lrrrrrrr",
  caption = "Pre-treatment country-level summary statistics: Ever-treated vs Never-treated"
) %>%
  kable_styling(latex_options = "hold_position", font_size = 10)


# 4 - DID estimations ------------------------------------------------------


#Uncontitional----------------------------
#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "notyettreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -5,
            max_e = 10)

summary(es)

ggdid(es)


p_es <- ggdid(es)

ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


# #outras janelas
# es <- aggte(att_gt_obj,
#             type = "dynamic",
#             min_e = -5,
#             max_e = 10)
# 
# summary(es)
# 
# ggdid(es)


#Event Studies
# robustness: nevertreated
att_gt_obj <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "nevertreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -5,
            max_e = 10)

summary(es)

ggdid(es)

p_es <- ggdid(es)

ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#check1

setorder(panel, Code, year)

panel[, post_adoption_zero := {
  if (all(is.na(first_treat_year))) FALSE
  else {
    t0 <- first_treat_year[1]
    if (is.na(t0)) FALSE
    else any(Reform.Dummy[year >= t0] == 0, na.rm = TRUE)
  }
}, by = Code]

panel[post_adoption_zero == TRUE, .(Country=unique(Country), 
                                    first_treat_year=unique(first_treat_year))][]

#check2

panel[, notyet := as.integer(gvar == 0 | year < gvar)]
panel[, .(n_notyet = uniqueN(Code[notyet==1]),
          n_treated_or_post = uniqueN(Code[notyet==0])),
      by = year][order(year)]

#check3

panel[gvar>0, .N, by=gvar][order(gvar)]

#check4

panel[gvar>0, .(n_countries = uniqueN(Code)), by=gvar][order(gvar)]


#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  xformla = ~ log_gdp_pc + trade_frac +
    gross_fixed_capital_frac + working_age_pop,
  data = panel,
  panel = TRUE,
  control_group = "notyettreated",
  est_method = "reg",
  faster_mode = FALSE
)

es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -5, max_e = 10)
summary(es_cond)

p_cond <- ggdid(es_cond)
p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  xformla = ~ log_gdp_pc + trade_frac +
    gross_fixed_capital_frac + working_age_pop,
  data = panel,
  panel = TRUE,
  control_group = "nevertreated",
  est_method = "reg",
  faster_mode = FALSE
)

es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -5, max_e = 10)
summary(es_cond)

p_cond <- ggdid(es_cond)
p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)

# 
# getS3method("fastglm", "formula", optional = TRUE)
#   packageVersion("fastglm")
#   library(fastglm)
#   
#   
#   R.version.string
#   Sys.which("make")

# 5 - Continuous Treatment effect ------------------------------------------------------
#Treatment is omega intensity

# dose no primeiro evento: pegue Omega no ano gvar (por país)
dose_dt <- panel[gvar > 0 & year == gvar,
                 .(dose = -Omega),
                 by = Code]



#View(panel[,list(Country, year, Reform.Dummy, treated_group)])

panel <- merge(panel, dose_dt, by = "Code", all.x = TRUE)
panel[gvar == 0, dose := 0]   # never-treated com dose 0

View(panel[,list(Country,Code, year, Reform.Dummy, gvar, dose, Omega)])

nrow(panel)
nrow(dt_es)

nrow(dt_es[complete.cases(
  share_income1,
  dose,
  log_gdp_pc,
  trade_frac,
  gross_fixed_capital_frac,
  working_age_pop
)])

panel[gvar > 0 & is.na(dose), .N]
panel[gvar == 0 & is.na(dose), .N]

summary(panel$dose)
table(is.na(panel$dose))
table(is.na(panel$share_income1))
table(is.na(panel$log_gdp_pc))
table(is.na(panel$trade_frac))
table(is.na(panel$gross_fixed_capital_frac))
table(is.na(panel$working_age_pop))


panel[gvar > 0, e := year - gvar]
panel[gvar == 0, e := NA_integer_]  # never-treated não tem event time

# manter janela
dt_es <- panel[ (gvar == 0) | (e >= -5 & e <= 10) ]

# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m <- feols(
  share_income1 ~ i(e, dose, ref = -1) + log_gdp_pc + trade_frac + 
    gross_fixed_capital_frac + working_age_pop | 
    Code + year,
  data = dt_es,
  cluster = "Code"
)

summary(m)

#plot
iplot(m)

png(file.path(figure_dir, "event_study_dose_share_income1.png"),
    width = 1600, height = 1000, res = 200)

iplot(m, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()