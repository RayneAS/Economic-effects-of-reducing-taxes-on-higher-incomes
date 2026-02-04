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
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/tax_reforms_AL"
  
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

# 2 - open income database (World Inequality Database) -------------------------

dt_income <- data.table(
  read_csv(
    file.path(data_dir, "income_share1_WID.csv")))

dt_income <- dt_income[, ("Code") := NULL]

setorder(dt_income, Country, year)


unique_countries <- sort(unique(dt_income$Country))
unique_countries


setdiff(lac_countries, sort(unique(dt_income$Country)))

dt_income <- dt_income[, lac := as.numeric(Country%in%lac_countries)]
dt_income <- dt_income[lac == 1]



#merge data1
panel_data <- merge(dt_income, reform_countries, by = c("Country", "year"), 
                    all.x = TRUE)


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


#merge
panel_data_final <- merge(panel_data, dt_controls, by = c("Country", "year"), 
                    all.x = TRUE)

colnames(panel_data_final)

panel_data_final <- panel_data_final[, c("oecd") := NULL]