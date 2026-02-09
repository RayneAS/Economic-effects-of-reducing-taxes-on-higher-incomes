#### pacote do wid no github
### dicionário: https://wid.world/codes-dictionary/#packages

# install.packages("devtools")
# devtools::install_github("world-inequality-database/wid-r-tool")
# https://wid.world/document/distributional-national-accounts-dina-guidelines-2025-methods-and-concepts-used-in-the-world-inequality-database/


library(readxl)
library(tidyverse)
library(data.table)
library(wid)



setwd("~/tributação - Fabiana")

## Income pre tax and transfers
data_incshare_pretax <- download_wid(
  indicators = "sptinc",    # s = share, ptinc = pre-tax national income
  areas = "all",            
  years = 1960:2024,        
  perc = c(
    "p90p100",      # Top 10%
    "p95p100",      # Top 5%
    "p99p100",      # Top 1%
    "p99.5p100",    # Top 0.5%
    "p99.9p100",    # Top 0.1%
    "p99.99p100",   # Top 0.01%
    "p0p10"),       # Bottom 10%
  metadata = TRUE, ages = 999, pop = "j") #all ages// pop- equally split between adults

data_incshare_pretax <- data.table(data_incshare_pretax)
data_incshare_pretax <- data_incshare_pretax[nchar(country) == 2] ## country-codes bigger than 2 digits are world or country regions
data_incshare_pretax <- unique(data_incshare_pretax) # remove duplicate data
data_incshare_pretax <- dcast(data_incshare_pretax, country + countryname + year + percentile + shortname  ~ variable, value.var = "value")
data_incshare_pretax <- data_incshare_pretax[, percentile := paste0("sptinc_", percentile)] 
data_incshare_pretax <- dcast(data_incshare_pretax, country + countryname + year + shortname ~ percentile, value.var = "sptinc999j")




## Disposable Income
data_incshare_dispinc <- download_wid(
  indicators = "sdiinc",    # s = share, diinc = disposable income//post-tax national income
  areas = "all",              
  years = 1960:2024,        
  perc = c(
    "p90p100",      # Top 10%
    "p95p100",      # Top 5%
    "p99p100",      # Top 1%
    "p99.5p100",    # Top 0.5%
    "p99.9p100",    # Top 0.1%
    "p99.99p100",   # Top 0.01%
    "p0p10"),       # Bottom 10%
  metadata = TRUE, ages = 999, pop = "j") #all ages// pop- equally split between adults

data_incshare_dispinc <- data.table(data_incshare_dispinc)
data_incshare_dispinc <- data_incshare_dispinc[nchar(country) == 2] ## country-codes bigger than 2 digits are world or country regions
data_incshare_dispinc <- unique(data_incshare_dispinc) # remove duplicate data
data_incshare_dispinc <- dcast(data_incshare_dispinc, country + countryname + year + percentile + shortname ~ variable, value.var = "value")
data_incshare_dispinc <- data_incshare_dispinc[, percentile := paste0("sdinc_", percentile)] 
data_incshare_dispinc <- dcast(data_incshare_dispinc, country + countryname + year +shortname ~ percentile, value.var = "sdiinc999j")




income <- merge(data_incshare_dispinc, data_incshare_pretax, by = c("country", "countryname", "year"), all = TRUE) 
fwrite(income, "incomeshare-wid.csv")

income$shortname.x <- NULL
income$shortname.y <- NULL

### with Gini coef
# gini_swiid <- data.table(read_csv("swiid9_91_summary.csv"))

gini_pt <- download_wid(
  indicators = "gptinc",    # gini coef = share, diinc = pre-tax national income
  areas = "all",              
  years = 1960:2024,        
  perc = "p0p100",
  ages = 999,               #all ages
  metadata = TRUE, 
  pop = "j")                #equally split between adults

gini_pt <- data.table(gini_pt)
gini_pt <- gini_pt[nchar(country) == 2]
#unique(gini_pt$country)
gini_pt <- dcast(gini_pt, country + countryname + year ~ variable, value.var = "value")


gini_di <- download_wid(
  indicators = "gdiinc",    # gini coef = share, diinc = pre-tax national income
  areas = "all",              
  years = 1960:2024,        
  perc = "p0p100",
  ages = 999,               #all ages
  metadata = TRUE, 
  pop = "j")                #equally split between adults

gini_di <- data.table(gini_di)
gini_di <- gini_di[nchar(country)==2]
# barplot(table(gini_di$year)) ## só a partir de 1980
# length(unique(gini_di$country))
gini_di <- dcast(gini_di, country + countryname + year ~ variable, value.var = "value")

fwrite(gini_di, "gini_disposableincome-wdi.csv")
fwrite(gini_pt, "gini_pretaxincome-wdi.csv")

### merge all inequality variables

ineq <- merge(income, gini_pt, by = c("country", "countryname", "year"))
ineq <- merge(ineq, gini_di, by = c("country", "countryname", "year"), all.x = T)
setnames(ineq, old = c("gdiinc999j", "gptinc999j"),
               new = c("gini_dinc", "gini_ptinc"))

fwrite(ineq, "inequality_data-wid.csv")
