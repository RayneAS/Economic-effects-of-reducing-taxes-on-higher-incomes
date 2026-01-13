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

#open trade countries-------------- -----------------------------------------------
trade <- data.table(
  read_csv(
    file.path(data_dir, "trade.csv"),
    skip = 3
  )
)

#open trade countries-------------- -----------------------------------------------
tax_revenue <- data.table(
  read_csv(
    file.path(data_dir, "tax_revenue.csv"),
    skip = 3
  )
)


#open gross domestic savings countries-------------- -----------------------------------------------
gross_savings <- data.table(
  read_csv(
    file.path(data_dir, "gross_dom_savings.csv"),
    skip = 3
  )
)


#open trade union_density countries-------------- -----------------------------------------------
union_density <- data.table(
  read_csv(
    file.path(data_dir, "union_density.csv")))


unique_year <- unique(union_density$TIME_PERIOD)
unique_year

#open working age population countries-------------- -----------------------------------------------
working_age_pop <- data.table(
  read_csv(
    file.path(data_dir, "working_age_pop.csv")))



