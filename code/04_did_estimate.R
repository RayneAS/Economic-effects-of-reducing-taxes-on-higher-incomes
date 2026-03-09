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



# 2 - Define auxiliary vars to did package ------------------------------------------------------

setorder(panel, Code, year)

# ever-treated indicator (country-level)
panel[, treated_group := as.integer(any(Reform.Dummy == 1, na.rm = TRUE)), 
      by = Code]

#View(panel[,list(Country, year, Reform.Dummy, treated_group)])
panel[treated_group == 1, uniqueN(Country)]
panel[, .N, by = treated_group]
panel[treated_group == 1, uniqueN(Code)]

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


# garantir que gvar seja double
panel[, gvar := as.numeric(gvar)]

# checagens
str(panel$gvar)
panel[, .(
  n_na_gvar = sum(is.na(gvar)),
  n_inf_gvar = sum(is.infinite(gvar)),
  min_gvar = min(gvar, na.rm = TRUE),
  max_gvar = max(gvar, na.rm = TRUE)
)]
sort(unique(panel$gvar))


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
  # outcome
  "pt_share_top1",
  "d_share_top1",
  "gini_pre_tax",
  "gini_post_tax",
  
  # controls
  "log_gdp_pc",
  "trade_frac",
  "tax_revenue_frac",
  "gross_fixed_capital_frac",
  "working_age_pop"
)

#define labels
var_labels <- c(
  pt_share_top1           = "Top 1% income share (pre-tax)",
  d_share_top1            = "Top 1% income share (post-tax)",
  gini_pre_tax            = "Gini coefficient (pre-tax income)",
  gini_post_tax           = "Gini coefficient (pos-tax income)",
  Reform.Dummy            = "Tax reform indicator",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac              = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation",
  working_age_pop         = "Working-age population"
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

#check1

panel[, notyet := as.integer(gvar == 0 | year < gvar)]
panel[, .(n_notyet = uniqueN(Code[notyet==1]),
          n_treated_or_post = uniqueN(Code[notyet==0])),
      by = year][order(year)]

#check2

panel[gvar>0, .N, by=gvar][order(gvar)]

#check3

panel[gvar>0, .(n_countries = uniqueN(Code)), by=gvar][order(gvar)]


#Create table to report check 3
# treatment cohorts
cohort_treated <- panel[gvar > 0,
                        .(Treated_countries = uniqueN(Code)),
                        by = gvar]

# number of not-yet-treated countries in each year
controls_year <- panel[, .(Control_countries = uniqueN(Code[notyet == 1])),
                       by = year]

# merge the two
cohort_table_full <- merge(cohort_treated,
                           controls_year,
                           by.x = "gvar",
                           by.y = "year",
                           all.x = TRUE)

setnames(cohort_table_full, "gvar", "Reform_year")

cohort_table_full <- cohort_table_full[order(Reform_year)]

#latex
kbl(
  cohort_table_full,
  format = "latex",
  booktabs = TRUE,
  align = "lrr",
  caption = "Distribution of tax reform adoption and available control countries"
) %>%
  kable_styling(latex_options = "hold_position", font_size = 10)


# 4.1 measure of inequality: pt_share_top1-------------------------------


#Unconditional----------------------------


#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "pt_share_top1",
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


p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated

att_gt_obj <- att_gt(
  yname = "pt_share_top1",
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

p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "pt_share_top1",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "pt_share_top1",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)


# 4.2 measure of inequality: d_share_top1-------------------------------


#Unconditional----------------------------


#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "d_share_top1",
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


p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_pos_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated
att_gt_obj <- att_gt(
  yname = "d_share_top1",
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

p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_pos_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "d_share_top1",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_pos_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "d_share_top1",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_pos_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)


# 4.3 measure of inequality: gini_pre_tax-------------------------------


#Unconditional----------------------------


#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "gini_pre_tax",
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


p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_gini_pre_tax_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated
att_gt_obj <- att_gt(
  yname = "gini_pre_tax",
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

p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_gini_pre_tax_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "gini_pre_tax",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_gini_pre_tax_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "gini_pre_tax",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_gini_pre_tax_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)

# 4.4 measure of inequality: gini_post_tax-------------------------------


#Unconditional----------------------------


#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "gini_post_tax",
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


p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_gini_post_tax_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated
att_gt_obj <- att_gt(
  yname = "gini_post_tax",
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

p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_gini_post_tax_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "gini_post_tax",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_gini_post_tax_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "gini_post_tax",
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

p_cond <- ggdid(es_cond) +
  labs(title = NULL)

p_cond


ggsave(file.path(figure_dir, "event_study_gini_post_tax_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)

