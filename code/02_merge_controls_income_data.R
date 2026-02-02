rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))


oecd_countries <- c(
  "Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica",
  "Czechia","Denmark","Estonia","Finland","France","Germany","Greece",
  "Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia",
  "Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway",
  "Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden",
  "Switzerland","Türkiye","United Kingdom","United States"
)

#PACKAGES USED
library(data.table)
library(readr)


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")


# 1 - open panel data------------------------------------------------------------------
panel_oecd <- data.table(
  read_csv(
    file.path(data_dir, "control_variables_oecd_countries.csv")))

panel_oecd  <- as.data.table(panel_oecd)


head(panel_oecd)
colnames(panel_oecd)

# 2 - Create growth rate for some variables--------------------------------------------
setorder(panel_oecd, Code, year)

#check duplicates
panel_oecd[, .N, by = .(Code, year)][N > 1]

#2.1.growth rate gdp per capita
class(panel_oecd$gdp_pc)
panel_oecd[, g_gdp_pc := (gdp_pc - data.table::shift(gdp_pc)) / 
             data.table::shift(gdp_pc), by = Code]

#View(panel_oecd[, list(Country, year, gdp_pc, g_gdp_pc)])

#2.2.growth rate patents
panel_oecd[, g_patent := (patent - data.table::shift(patent)) / 
             data.table::shift(patent), by = Code]

#View(panel_oecd[, list(Country, year, patent, g_patent)])

#2.3.growth rate government gross debt
panel_oecd[, g_gov_g_debt := (gov_gross_debt - data.table::shift(gov_gross_debt)) / 
             data.table::shift(gov_gross_debt), by = Code]

#View(panel_oecd[, list(Country, year, gov_gross_debt, g_gov_g_debt)])


# 3 - Open rubolino database --------------------------------------------
dt2 <- data.table(
  read_xls(
    file.path(data_dir, "dataset_2.xls")))


dt2 <- dt2[, .SD,
           .SDcols = c("Year", "Country", "Code","Reform Dummy", "Significant Reform", 
                       "Tax Brackets")]

omega <- data.table(
  read_xls(
    file.path(data_dir, "omega.xls")))

omega <- omega[, .SD,
           .SDcols = c("Year", "Country", "Code","ATRtop1", "ATRavg",
                       "ARP", "Omega")]

rubolino <- merge(dt2, omega, by = c("Year","Code","Country"), all.x = TRUE)

setorder(rubolino, Country, Year)

setdiff(oecd_countries, sort(unique(rubolino$Country)))

