rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "haven",
  "knitr",
  "kableExtra",
  "did",
  "ggplot2", 
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
library(haven)
library(knitr)
library(kableExtra)
library(did)
library(ggplot2)
library(fixest)

# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/tax_reforms_AL"
  
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output_artigo2")



# 1 - open data-------------------------------------------------------------
panel <- data.table(
  read_csv(
    file.path(data_dir, "LA_data_for_model.csv")))

colnames(panel)

unico_country <- unique(panel$Country)
unico_country


# 2 - Descriptive Analysis total sample  --------------------------------------------

#define variables
vars_desc <- c(
  # outcome
  "pt_share_top1",
  "d_share_top1",
  "gini_pre_tax",
  "gini_post_tax",
  
  # controls
  "log_gdp_pc",
  "trade_frac",
  "tax_revenue_frac",
  "gross_fixed_capital_frac"
)

#define function
desc_table <- panel[
  , lapply(.SD, function(x) {
    c(
      N    = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD   = sd(x, na.rm = TRUE),
      Min  = min(x, na.rm = TRUE),
      Max  = max(x, na.rm = TRUE)
    )
  }),
  .SDcols = vars_desc
]

desc_table <- as.data.table(t(desc_table), keep.rownames = "Variable")

setnames(
  desc_table,
  c("Variable", "N", "Mean", "SD", "Min", "Max")
)

desc_table

#define labels
var_labels <- c(
  pt_share_top1           = "Top 1% income share (pre-tax)",
  d_share_top1            = "Top 1% income share (post-tax)",
  gini_pre_tax            = "Gini coefficient (pre-tax income)",
  gini_post_tax           = "Gini coefficient (post-tax income)",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac              = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation"
)

desc_table[, Variable := var_labels[Variable]]

#get latex code
kbl(
  desc_table,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  caption = "Descriptive statistics"
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  )


# 3 - Define auxiliary vars to did package ------------------------------------------------------

setorder(panel, Code, year)

#the treatment is structural 

#ever-treated: any tax reform (structural )
panel[, treated_group := as.integer(any(structural  > 0, na.rm = TRUE)), 
      by = Code]

panel[treated_group == 1, uniqueN(Country)]
panel[, .N, by = treated_group]
panel[treated_group == 1, uniqueN(Code)]


# first year of any tax reform
panel[, first_treat_year := 
        if (any(structural  > 0, na.rm = TRUE)) {
          min(year[structural  > 0], na.rm = TRUE)
        } else {
          NA_integer_
        },
      by = Code]


panel[Country=="Argentina",
      .(first_treat_year=unique(first_treat_year))]

# Define pre-period flag
panel[, pre_period := 0L]
panel[treated_group == 1 & !is.na(first_treat_year) & 
        year < first_treat_year, pre_period := 1L]
panel[treated_group == 0, pre_period := 1L]


# View(panel[,list(Country, year, structural  ,structural , 
#                  pre_period, treated_group, first_treat_year)])

#Define country numeric id did package
panel[, id := .GRP, by = Code]

#Define gvar did package
panel[, g_var := first_treat_year]
panel[is.na(g_var), g_var := 0]


# checagens
str(panel$g_var)
panel[, .(
  n_na_gvar = sum(is.na(g_var)),
  n_inf_gvar = sum(is.infinite(g_var)),
  min_gvar = min(g_var, na.rm = TRUE),
  max_gvar = max(g_var, na.rm = TRUE)
)]
sort(unique(panel$g_var))

#data checks
panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[g_var > 0]),
  n_never = uniqueN(id[g_var == 0])
)]

# test <- panel[treated_group==1]
# unique_countri <- unique(test$Country)
# unique_countri

panel[, uniqueN(g_var)]
unique_g_var <- sort(unique(panel$g_var))
unique_g_var

#check g_var
check_gvar <- panel[g_var > 0,
                    .(
                      gvar_unique = unique(g_var),
                      min_year_treated = min(year[structural  == 1], 
                                             na.rm = TRUE)
                    ),
                    by = .(Code, Country)
]

check_gvar[gvar_unique != min_year_treated]

panel[, .N, by = .(Code, year)][N > 1]
panel[, .N, by = Code][order(N)]

# 4 - Baseline (pre-treatment) summary stats at COUNTRY level------------------------------------------

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
  "gross_fixed_capital_frac"
)

#define labels
var_labels <- c(
  pt_share_top1           = "Top 1% income share (pre-tax)",
  d_share_top1            = "Top 1% income share (post-tax)",
  gini_pre_tax            = "Gini coefficient (pre-tax income)",
  gini_post_tax           = "Gini coefficient (post-tax income)",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac              = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation"
)

missing_vars <- setdiff(vars_baseline, names(panel))
if (length(missing_vars) > 0) {
  stop("Variáveis ausentes em vars_baseline: ", paste(missing_vars, collapse = ", "))
}


#ever-treated: years < g_var
#never-treated: in all years in the sample
panel_pre <- panel[(g_var == 0L) | (year < g_var)]


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


# 5- measure of inequality: pt_share_top1-------------------------------
#Checks

panel[g_var > 0, .(n_countries = uniqueN(Code)), by = g_var][order(g_var)]

panel[, .(first_obs = min(year), g = unique(g_var)), by = .(Code, Country)][order(g)]


# outcome
# "pt_share_top1",
# "d_share_top1",
# "gini_pre_tax",
# "gini_post_tax",

#Unconditional----------------------------

#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "g_var",
  data = panel,
  panel = TRUE,
  control_group = "notyettreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -3,
            max_e = 5)

summary(es)

ggdid(es)


p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated_LA.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated

att_gt_obj <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "g_var",
  data = panel,
  panel = TRUE,
  control_group = "nevertreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -3,
            max_e = 5)

summary(es)

ggdid(es)

p_es <- ggdid(es) +
  labs(title = NULL)

ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated_LA.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

# att_gt_cond <- att_gt(
#   yname = "d_share_top1",
#   tname = "year",
#   idname = "id",
#   gname = "g_var",
#   xformla = ~ log_gdp_pc + trade_frac + 
#     gross_fixed_capital_frac ,
#   data = panel,
#   panel = TRUE,
#   control_group = "notyettreated",
#   est_method = "reg",
#   faster_mode = FALSE
# )
# 
# es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -3, max_e = 5)
# summary(es_cond)
# 
# p_cond <- ggdid(es_cond) +
#   labs(title = NULL)
# 
# p_cond
# 
# 
# ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated_cond_LA.jpg"), 
#        plot = p_cond,
#        height= 4, width = 6)
# 
# 
# 
# #Event Studies
# # robustness: nevertreated
# 
# att_gt_cond <- att_gt(
#   yname = "d_share_top1",
#   tname = "year",
#   idname = "id",
#   gname = "g_var",
#   xformla = ~ log_gdp_pc + trade_frac +
#     gross_fixed_capital_frac ,
#   data = panel,
#   panel = TRUE,
#   control_group = "nevertreated",
#   est_method = "reg",
#   faster_mode = FALSE
# )
# 
# es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -3, max_e = 5)
# summary(es_cond)
# 
# p_cond <- ggdid(es_cond) +
#   labs(title = NULL)
# 
# p_cond
# 
# 
# ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated_cond_LA.jpg"), 
#        plot = p_cond,
#        height= 4, width = 6)


##TWFE------------------------------------------------------------------

# post-treatment indicator
panel[, post := 0L]
panel[treated_group == 1 & year >= first_treat_year, post := 1L]

# DID interaction
panel[, did := treated_group * post]

# checagens
table(panel$post, useNA = "ifany")
table(panel$did, useNA = "ifany")

panel[, .(
  first_treat_year = unique(first_treat_year),
  treated_group = unique(treated_group),
  min_post = min(post),
  max_post = max(post)
), by = Code][order(first_treat_year)]

twfe_1 <- feols(
  pt_share_top1 ~ did | Code + year,
  data = panel,
  cluster = ~Code
)

summary(twfe_1)

