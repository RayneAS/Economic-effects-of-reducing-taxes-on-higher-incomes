rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "haven",
  "knitr",
  "kableExtra"
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


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/tax_reforms_AL"
  
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")



# 1 - open data-------------------------------------------------------------
panel <- data.table(
  read_csv(
    file.path(data_dir, "LA_data_for_model.csv")))

colnames(panel)

unico_country <- unique(panel$Country)
unico_country

# Log transformations
panel[, log_gdp_pc := log(gdp_pc)]
panel[, log_patent := log(1 + patent)]
panel[, log_pt_share_top1 := log(pt_share_top1)]
panel[, log_d_share_top1 := log(d_share_top1)]


# Variables originally in percent or % of GDP
pct_vars <- c(
  "trade",
  "tax_revenue",
  "gross_savings",
  "gross_fixed_capital",
  "bank_deposits_to_gdp",
  "stocks_capt",
  "stocks_trade",
  "trade_union",
  "gov_gross_debt"
)

for (v in pct_vars) {
  panel[, paste0(v, "_frac") := get(v) / 100]
}

panel[, working_age_pop := working_age_pop / 100]


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
  "gross_fixed_capital_frac",
  "working_age_pop"
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
  Reform.Dummy            = "Tax reform indicator",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac              = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation",
  working_age_pop         = "Working-age population"
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
