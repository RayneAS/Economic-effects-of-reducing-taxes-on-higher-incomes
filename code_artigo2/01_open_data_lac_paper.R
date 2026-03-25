#Source: https://data.iadb.org/pt-BR/dataset/tax-reforms-in-latin-america-in-an-era-of-democracy-a-database-2014-update
#Download dta using above data source

rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "haven"
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
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/tax_reforms_AL"
  
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")

#World Bank (Latin America & Caribbean – LAC)
#List LAC countries

lac_countries <- c(
  # América do Sul
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Ecuador",
  "Guyana",
  "Paraguay",
  "Peru",
  "Suriname",
  "Uruguay",
  "Venezuela",
  
  # América Central + México
  "Mexico",
  "Costa Rica",
  "El Salvador",
  "Guatemala",
  "Honduras",
  "Nicaragua",
  "Panama",
  "Belize",
  
  # Caribe
  "Bahamas",
  "Barbados",
  "Cuba",
  "Dominican Republic",
  "Haiti",
  "Jamaica",
  "Trinidad and Tobago",
  "Grenada",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Antigua and Barbuda",
  "Saint Kitts and Nevis"
)


# 1 - open raw data-------------------------------------------------------------

reform_countries <- data.table(
  read_dta(
    file.path(data_dir2, "fsh-taxreformslac-v02-2014-stata13.dta")))

setnames(reform_countries, "country", "Country")
colnames(reform_countries)

reform_countries <- reform_countries[, .(Country, year,TaxRefOverhaul,
                                         TaxRefAdmReform, TaxRefPITBroad,
                                         TaxRefPITRate)]


# 2 - Income database (World Inequality Database) -----------------------------

#Inequality data it was cleaned and organized by Mariana in another code 

dt_income <- data.table(
  read_csv(
    file.path(data_dir, "final_data_inequality_WID.csv")))

colnames(dt_income)
dt_income <- dt_income[, ("Code") := NULL]


dt_income[, year := as.integer(year)]

setorder(dt_income, Country, year)

stopifnot(is.integer(dt_income$year))

unique_countries <- sort(unique(dt_income$Country))
unique_countries

unique_countries <- sort(unique(reform_countries$Country))
unique_countries

setdiff(lac_countries, sort(unique(dt_income$Country)))


dt_income <- dt_income[, lac := as.numeric(Country%in%lac_countries)]
dt_income <- dt_income[lac == 1]


range(dt_income$year)
range(reform_countries$year)

dt_income <- dt_income[year >= 1990 & year <= 2004]


#merge data1
panel_data <- merge(dt_income, reform_countries, by = c("Country", "year"), 
                    all.x = TRUE)

#obs:
# Missing values in reform variables are recoded as 0.
# This assumes that country-years without a recorded reform in the source data
# correspond to no reform occurrence, rather than missing information.

vars <- c("TaxRefOverhaul", "TaxRefAdmReform", 
          "TaxRefPITBroad", "TaxRefPITRate")


panel_data[, (vars) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
           .SDcols = vars]

setorder(panel_data, Country, year)

# 3 - open control databases  --------------------------------------------------

dt_controls <- data.table(
  read_csv(
    file.path(data_dir, "control_variables_all_countries.csv")))

setdiff(lac_countries, sort(unique(dt_controls$Country)))

unique_countries <- sort(unique(dt_controls$Country))
unique_countries


dt_controls[, Country := fcase(
  Country == "Venezuela, RB", "Venezuela",
  Country == "Bahamas, The", "Bahamas",
  Country == "St. Lucia", "Saint Lucia",       
  Country == "St. Kitts and Nevis", "Saint Kitts and Nevis",
  Country == "St. Vincent and the Grenadines", "Saint Vincent and the Grenadines",
  default = Country
)]

setdiff(lac_countries, sort(unique(dt_controls$Country)))

dt_controls <- dt_controls[year >= 1990 & year <= 2004]


#merge
panel_data_final <- merge(panel_data, dt_controls, by = c("Country", "year"), 
                    all.x = TRUE)


## 4- Define treatment -------------------------------------------------------------
#rename vars 
setnames(panel_data_final,
         old = c(
           "TaxRefOverhaul",
           "TaxRefAdmReform",
           "TaxRefPITBroad",
           "TaxRefPITRate"),
         new = c(
           "Ref_Overhaul",
           "Ref_AdmReform",
           "Ref_PITBroad",
           "Ref_PITRate"))

table(panel_data_final$Ref_Overhaul, useNA = "ifany")
table(panel_data_final$Ref_AdmReform, useNA = "ifany")
table(panel_data_final$Ref_PITBroad, useNA = "ifany")
table(panel_data_final$Ref_PITRate, useNA = "ifany")


#define increase tax reforms
panel_data_final[, tax_increase := as.integer(Ref_PITRate == 1 | Ref_PITBroad == 1)]

#define cut tax reforms
panel_data_final[, tax_cut:= as.integer(Ref_PITRate == -1 | Ref_PITBroad == -1)]


#define structural reforms
panel_data_final[, structural:= as.integer(Ref_Overhaul == 2 | Ref_AdmReform == 1)]

table(panel_data_final$tax_increase, useNA = "ifany")
table(panel_data_final$tax_cut, useNA = "ifany")
table(panel_data_final$structural, useNA = "ifany")


#check1
panel_data_final[, overlap_inc_cut := tax_increase + tax_cut]
table(panel_data_final$overlap_inc_cut)

#check2
colSums(panel_data_final[, .(tax_increase, tax_cut, structural,
                             overlap_inc_cut)], na.rm = TRUE)



panel_data_final[, g_increase :=
                   ifelse(any(tax_increase == 1, na.rm = TRUE), min(year[tax_increase == 1]), 0),
                 by = Country
]

panel_data_final[, g_cut :=
                   ifelse(any(tax_cut == 1, na.rm = TRUE), min(year[tax_cut == 1]), 0),
                 by = Country
]

panel_data_final[, g_struct :=
                   ifelse(any(structural == 1, na.rm = TRUE), min(year[structural == 1]), 0),
                 by = Country
]

panel_data_final[, g_overlap :=
                   ifelse(any(overlap_inc_cut == 1, na.rm = TRUE), min(year[overlap_inc_cut == 1]), 0),
                 by = Country
]

table(panel_data_final$g_increase)
table(panel_data_final$g_cut)
table(panel_data_final$g_struct)
table(panel_data_final$g_overlap)


panel_data_final[g_increase > 0, .N, by = Country]
panel_data_final[g_cut > 0, .N, by = Country]
panel_data_final[g_struct > 0, .N, by = Country]


panel_data_final[, oecd := NULL]

## 5- Final adjustments -------------------------------------------------------------

# Log transformations
panel_data_final[, log_gdp_pc := log(gdp_pc)]
panel_data_final[, log_patent := log(1 + patent)]
panel_data_final[, log_pt_share_top1 := log(pt_share_top1)]
panel_data_final[, log_d_share_top1 := log(d_share_top1)]


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
  panel_data_final[, paste0(v, "_frac") := get(v) / 100]
}

panel_data_final[, working_age_pop := working_age_pop / 100]

## 6- save data -------------------------------------------------------------

fwrite(panel_data_final,
       file.path(data_dir, paste0("LA_data_for_model.csv")),
       sep = ",")
