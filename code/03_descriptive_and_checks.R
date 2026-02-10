rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl"
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


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")


# 1 - open data and run checks------------------------------------------------------------------
panel <- data.table(
  read.csv(
    file.path(data_dir,"complete_dataset.csv" )
  )
)

colnames(panel)

# 1.1 - Check duplicates  
key_vars <- c("Code","Country","year")

dup_key <- panel[, .N, by = key_vars][N > 1][order(-N)]
if (nrow(dup_key) > 0) {
  message("There are duplicates in panel data (Code-Country-year)")
  print(head(dup_key, 20))
}


# 1.2 - Check the quality of the data 

#for numeric vars
num_vars <- setdiff(names(panel), c("Code","Country","year"))

is_num <- function(x) is.numeric(x) || is.integer(x)

var_qc_numeric <- function(DT, v){
  x <- DT[[v]]
  finite_x <- x[is.finite(x)]
  n <- length(x)
  n_miss <- sum(is.na(x))
  n_inf  <- sum(is.infinite(x), na.rm = TRUE)
  n_nan  <- sum(is.nan(x), na.rm = TRUE)
  
  # statistics
  q <- if (length(finite_x) > 0) quantile(finite_x, 
  probs = c(.01,.05,.25,.5,.75,.95,.99), na.rm=TRUE) else rep(NA_real_, 7)
  names(q) <- c("p01","p05","p25","p50","p75","p95","p99")
  
  # check outliers 
  out_share <- NA_real_
  if (length(finite_x) >= 10) {
    iqr <- IQR(finite_x, na.rm = TRUE)
    if (is.finite(iqr) && iqr > 0) {
      lo <- q["p25"] - 3*iqr
      hi <- q["p75"] + 3*iqr
      out_share <- mean(finite_x < lo | finite_x > hi, na.rm = TRUE)
    } else out_share <- 0
  }
  
  #check the within country variation
  within_country <- DT[, .(sd_within = sd(get(v), na.rm=TRUE),
                           uniq_within = uniqueN(get(v))), by = .(Code, Country)]
  frac_const_by_country <- mean(within_country$uniq_within <= 1, na.rm = TRUE)
  
  data.table(
    variable = v,
    type = class(x)[1],
    N = n,
    miss_n = n_miss,
    miss_share = n_miss / n,
    inf_n = n_inf,
    nan_n = n_nan,
    mean = if (length(finite_x)>0) mean(finite_x, na.rm=TRUE) else NA_real_,
    sd   = if (length(finite_x)>1) sd(finite_x, na.rm=TRUE) else NA_real_,
    min  = if (length(finite_x)>0) min(finite_x, na.rm=TRUE) else NA_real_,
    max  = if (length(finite_x)>0) max(finite_x, na.rm=TRUE) else NA_real_,
    p01 = q["p01"], p05 = q["p05"], p25 = q["p25"], p50 = q["p50"],
    p75 = q["p75"], p95 = q["p95"], p99 = q["p99"],
    out_share_iqr3 = out_share,
    frac_const_by_country = frac_const_by_country
  )
}

#for categorical vars
var_qc_categorical <- function(DT, v, topn=10){
  x <- DT[[v]]
  n <- length(x)
  n_miss <- sum(is.na(x))
  tab <- DT[, .N, by = ..v][order(-N)]
  top <- tab[1:min(topn, .N)]
  data.table(
    variable = v,
    type = class(x)[1],
    N = n,
    miss_n = n_miss,
    miss_share = n_miss / n,
    n_unique = uniqueN(x),
    top_values = paste0(top[[v]], ":", top$N, collapse = " | ")
  )
}

#run functions
qc_list <- lapply(num_vars, function(v){
  if (is_num(panel[[v]])) var_qc_numeric(panel, v) else var_qc_categorical(panel, v)
})

qc <- rbindlist(qc_list, fill = TRUE)

qc_overview <- qc[order(-miss_share, variable)]
print(qc_overview)


# 1.3 - other Checks 

#dummy vars
dummy_vars <- intersect(c("Reform.Dummy","Significant.Reform"), names(panel))
for (v in dummy_vars){
  bad <- panel[!is.na(get(v)) & !(get(v) %in% c(0,1,TRUE,FALSE)), .N]
  if (bad > 0) message("ATENÇÃO: ", v, " tem valores fora de {0,1,TRUE,FALSE}.")
}

#share measure vars 
share_like <- intersect(c(
  "trade_frac",
  "tax_revenue_frac",
  "gross_savings_frac",
  "gross_fixed_capital_frac",
  "bank_deposits_to_gdp_frac",
  "stocks_capt_frac",
  "stocks_trade_frac",
  "gov_gross_debt_frac",
  "trade_union_frac",
  "share_income1"         
), names(panel))

flag_range <- rbindlist(lapply(share_like, function(v){
  x <- panel[[v]]
  finite_x <- x[is.finite(x)]
  if (length(finite_x)==0) return(data.table(variable=v, share_outside_0_1=NA_real_, note="sem dados finitos"))
  data.table(
    variable = v,
    share_outside_0_1 = mean(finite_x < 0 | finite_x > 1, na.rm=TRUE),
    note = "se estiver em % (0-100) ou pontos do PIB, ignorar este alerta"
  )
}))
print(flag_range[order(-share_outside_0_1)])


#check outlier in growth variation variables
growth_vars <- grep("^g_", names(panel), value = TRUE)

if (length(growth_vars) > 0){
  growth_check <- rbindlist(lapply(growth_vars, function(v){
    x <- panel[[v]]
    finite_x <- x[is.finite(x)]
    data.table(variable=v,
               min=min(finite_x, na.rm=TRUE),
               p01=quantile(finite_x, .01, na.rm=TRUE),
               p99=quantile(finite_x, .99, na.rm=TRUE),
               max=max(finite_x, na.rm=TRUE))
  }), fill=TRUE)
  print(growth_check[order(variable)])
}
