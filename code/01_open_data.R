rm(list = ls())
gc()

# # Install packages
# packages <- c(
#   "dplyr",
#   "data.table",
#   "tictoc",
#   "haven",
#   "questionr",
#   "tidyverse",
#   "ggplot2",
#   "ggthemes",
#   "cowplot",
#   "Hmisc",
#   "ggpubr",
#   "summarytools",
#   "kableExtra",
#   "psych",
#   "survey",
#   "readxl",
#   "readr",
#   "plm",
#   "lmtest",
#   "feather",
#   "xtable",
#   "geobr",
#   "stringr",
#   "stringdist",
#   "magrittr",
#   "devtools",
#   "SDMTools",
#   "rlang",
#   "reshape2",
#   "knitr"
# )
# 
# installed <- rownames(installed.packages())
# to_install <- setdiff(packages, installed)
# 
# if (length(to_install) > 0) {
#   install.packages(to_install, dependencies = TRUE)
# }
# 
# invisible(lapply(packages, library, character.only = TRUE))


#PACKAGES USED
library(dplyr)
library(data.table)
library(tictoc)
library(haven)
library(questionr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(Hmisc)
library(questionr)
library(ggpubr)
library(summarytools)
library(kableExtra)
library(psych)
library(survey)
library(readxl)
library(readr)
library(plm)
library(lmtest)
library (feather)
library(xtable)
library(geobr)
library(stringr)
library(stringdist)
library(magrittr)
library(devtools)
library(rlang)
library(reshape2)
library(xtable)
library(knitr)
library(kableExtra)

# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")


#open gdp countries-------------- -----------------------------------------------
gdp_pc <- data.table(
  read_csv(
    file.path(data_dir, "gdp_per_capita.csv"),
    skip = 3
  )
)

head(gdp_pc)

setnames(gdp_pc, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "GDPpercapita"))

gdp_pc$"Indicator Code" <- NULL


year_cols <- grep("^[0-9]{4}$", names(gdp_pc), value = TRUE)

# reshape to long
gdp_long <- melt(
  gdp_pc,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "gdp_pc",
  variable.factor = FALSE
)

head(gdp_long)

setorder(gdp_long, Country, year)

remove(gdp_pc)

#open trade countries-------------- -----------------------------------------------
trade <- data.table(
  read_csv(
    file.path(data_dir, "trade.csv"),
    skip = 3
  )
)

head(trade)

setnames(trade, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "trade_to_gdp"))

trade$"Indicator Code" <- NULL



year_cols <- grep("^[0-9]{4}$", names(trade), value = TRUE)


# reshape to long
trade_long <- melt(
  trade,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "trade",
  variable.factor = FALSE
)

head(trade_long)

setorder(trade_long, Country, year)

remove(trade)

#open trade countries-------------- -----------------------------------------------
tax_revenue <- data.table(
  read_csv(
    file.path(data_dir, "tax_revenue.csv"),
    skip = 3
  )
)

head(tax_revenue)

setnames(tax_revenue, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "tax_revenue_to_gdp"))

tax_revenue$"Indicator Code" <- NULL


year_cols <- grep("^[0-9]{4}$", names(tax_revenue), value = TRUE)


# reshape to long
tax_revenue_long <- melt(
  tax_revenue,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "tax_revenue",
  variable.factor = FALSE
)

head(tax_revenue_long)

setorder(tax_revenue_long, Country, year)

remove(tax_revenue)


#open gross domestic savings countries-------------- -----------------------------------------------
gross_savings <- data.table(
  read_csv(
    file.path(data_dir, "gross_dom_savings.csv"),
    skip = 3
  )
)

head(gross_savings)

setnames(gross_savings, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "tax_revenue_to_gdp"))

gross_savings$"Indicator Code" <- NULL


year_cols <- grep("^[0-9]{4}$", names(gross_savings), value = TRUE)


# reshape to long
gross_savings_long <- melt(
  gross_savings,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "gross_savings",
  variable.factor = FALSE
)

head(gross_savings_long)

setorder(gross_savings_long, Country, year)

remove(gross_savings)


#open gross fixed capital countries-------------- -----------------------------------------------
gross_fixed_capital <- data.table(
  read_csv(
    file.path(data_dir, "gross_fixed_capital.csv"),
    skip = 3
  )
)

head(gross_fixed_capital)

setnames(gross_fixed_capital, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "gross_fixed_capital"))

gross_fixed_capital$"Indicator Code" <- NULL


year_cols <- grep("^[0-9]{4}$", names(gross_fixed_capital), value = TRUE)


# reshape to long
gross_fixed_capital_long <- melt(
  gross_fixed_capital,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "gross_fixed_capital",
  variable.factor = FALSE
)

head(gross_fixed_capital_long)

setorder(gross_fixed_capital_long, Country, year)

remove(gross_fixed_capital)

#open trade union_density countries-------------- -----------------------------------------------
union_density <- data.table(
  read_csv(
    file.path(data_dir, "union_density.csv")))


head(union_density)

unique <- unique(union_density$ACTION)
unique

unique <- unique(union_density$MEASURE)
unique

unique <- unique(union_density$TIME_PERIOD)
unique

unique <- unique(union_density$UNIT_MEASURE)
unique

unique <- unique(union_density$`Unit of measure`)
unique

unique <- unique(union_density$`Time period`)
unique

unique <- unique(union_density$OBS_STATUS)
unique

unique <- unique(union_density$`Observation status`)
unique

unique <- unique(union_density$Decimals)
unique

unique <- unique(union_density$DECIMALS)
unique

unique <- unique(union_density$`Unit multiplier`)
unique

unique <- unique(union_density$UNIT_MULT)
unique

unique <- unique(union_density$`Observation value`)
unique

unique <- unique(union_density$OBS_VALUE)
unique

unique <- unique(union_density$Measure)
unique


setnames(union_density, c("Reference area", "REF_AREA", "OBS_VALUE", "TIME_PERIOD"), 
         c("Country", "Code", "trade_union","year"))

union_density[, c("STRUCTURE", "STRUCTURE_ID", "ACTION", "MEASURE",
                  "UNIT_MEASURE", "Unit of measure","Time period",
                  "OBS_STATUS","Observation status","Decimals",
                  "DECIMALS", "Unit multiplier", "UNIT_MULT",
                  "Observation value","STRUCTURE_NAME","Measure") := NULL]

unique <- unique(union_density$Country)
unique

#drop not oecd countries
not_oecd <- c(
  "India",
  "Indonesia",
  "Montenegro",
  "OECD"
)

union_density <- union_density[!Country %in% not_oecd]
union_density <- union_density[, oecd:= 1]

#open working age population countries-------------- -----------------------------------------------
working_age_pop <- data.table(
  read_csv(
    file.path(data_dir, "working_age_pop.csv")))


head(working_age_pop)

unique <- unique(working_age_pop$ACTION)
unique

unique <- unique(working_age_pop$MEASURE)
unique

unique <- unique(working_age_pop$TIME_PERIOD)
unique

unique <- unique(working_age_pop$UNIT_MEASURE)
unique

unique <- unique(working_age_pop$`Unit of measure`)
unique

unique <- unique(working_age_pop$`Time period`)
unique

unique <- unique(working_age_pop$OBS_STATUS)
unique

unique <- unique(working_age_pop$`Observation status`)
unique

unique <- unique(working_age_pop$Decimals)
unique

unique <- unique(working_age_pop$DECIMALS)
unique

unique <- unique(working_age_pop$`Unit multiplier`)
unique

unique <- unique(working_age_pop$UNIT_MULT)
unique

unique <- unique(working_age_pop$`Observation value`)
unique

unique <- unique(working_age_pop$Measure)
unique

unique <- unique(working_age_pop$`Time horizon`)
unique

unique <- unique(working_age_pop$TIME_HORIZ)
unique

unique <- unique(working_age_pop$Age)
unique

unique <- unique(working_age_pop$AGE)
unique

unique <- unique(working_age_pop$Sex)
unique


unique <- unique(working_age_pop$SEX)
unique


setnames(working_age_pop, c("Reference area", "REF_AREA", "OBS_VALUE", "TIME_PERIOD"), 
         c("Country", "Code", "working_age_pop","year"))

working_age_pop[, c("STRUCTURE", "STRUCTURE_ID", "ACTION", "MEASURE",
                  "UNIT_MEASURE", "Unit of measure","Time period",
                  "OBS_STATUS","Observation status","Decimals",
                  "DECIMALS", "Unit multiplier", "UNIT_MULT",
                  "Observation value","STRUCTURE_NAME","Measure",
                  "Time horizon","TIME_HORIZ", "AGE", "Age",
                  "Sex", "SEX") := NULL]

unique <- unique(working_age_pop$Country)
unique

#drop not oecd countries and others
not_oecd <- c(
  "Argentina",
  "Brazil",
  "Bulgaria",
  "China (People’s Republic of)",
  "Croatia",
  "Cyprus",
  "India",
  "Indonesia",
  "Romania",
  "Russia",
  "Saudi Arabia",
  "Singapore",
  "South Africa",
  "Malta",
  "G20",
  "European Union (27 countries)",
  "OECD",
  "World"
)

working_age_pop <- working_age_pop[!Country %in% not_oecd]
working_age_pop <- working_age_pop[, oecd:= 1]

setorder(working_age_pop, Country, year)



