rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "knitr",
  "kableExtra",
  "did"
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




# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")


# 1 - open data ----------------------------------------------------------------
panel <- data.table(
  read.csv(
    file.path(data_dir,"complete_dataset.csv" )
  )
)

colnames(panel)

panel <- panel[year >= 1980]


# 2 - Check duplicates ----------------------------------------------------------------
  
key_vars <- c("Code","Country","year")

dup_key <- panel[, .N, by = key_vars][N > 1][order(-N)]
if (nrow(dup_key) > 0) {
  message("There are duplicates in panel data (Code-Country-year)")
  print(head(dup_key, 20))
}


# 3 - Check the quality of the data ----------------------------------------------------------------

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


# 4 - Other Checks  ----------------------------------------------------------------

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
    note = "If it is in % (0-100) or points of GDP, ignore"
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

# 5 - Missing Analysis  -------------------------------------------------------

# missing by variable and year 
miss_by_year <- panel[, lapply(.SD, function(x) mean(is.na(x))), 
                      by = year, .SDcols = num_vars]

# missing by country 
miss_by_country <- panel[, lapply(.SD, function(x) mean(is.na(x))), 
                         by = .(Code, Country), .SDcols = num_vars]
# check gdp_pc
#miss_by_country[order(-gdp_pc)][1:10]


# 6 - Check for within country variation  --------------------------------------

#frac_sd_zero: fração de países onde o desvio-padrão ao longo do tempo é zero
#median_sd_within: desvio-padrão mediano dentro do país

within_sd <- rbindlist(lapply(intersect(num_vars, names(panel)), function(v){
  if (!is_num(panel[[v]])) return(NULL)
  tmp <- panel[, .(sd_within = sd(get(v), na.rm=TRUE)), by=.(Code, Country)]
  data.table(variable=v,
             frac_sd_zero = mean(tmp$sd_within==0, na.rm=TRUE),
             median_sd_within = median(tmp$sd_within, na.rm=TRUE))
}), fill=TRUE)

within_sd[, `:=`(
  frac_sd_zero_pct = round(100 * frac_sd_zero, 1),
  median_sd_within = round(median_sd_within, 4)
)]

print(within_sd[order(-frac_sd_zero)])


# 7 - Descriptive Analysis total sample  --------------------------------------------

#define variables
vars_desc <- c(
  # outcome
  "share_income1",
  
  # treatment
  "Reform.Dummy",
  
  # controls
  "log_gdp_pc",
  "trade_frac",
  "tax_revenue_frac",
  "gross_fixed_capital_frac",
  "working_age_pop",
  "log_patent"
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
  share_income1           = "Top 1% income share",
  Reform.Dummy            = "Tax reform indicator",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac         = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation",
  working_age_pop         = "Working-age population",
  log_patent              = "Log patent applications"
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

#clean
rm(dup_key,
   flag_range,
   growth_check,
   miss_by_country,
   miss_by_year,
   qc,
   qc_list,
   qc_overview,
   within_sd)

# 8 - Define treatment and control groups ------------------------------------------------------

# ever-treated indicator (country-level)
panel[, treated_group := as.integer(any(Reform.Dummy == 1, na.rm = TRUE)), 
      by = Code]


View(panel[,list(Country, year, Reform.Dummy, treated_group)])
panel[treated_group == 1, uniqueN(Country)]

# 1) Função de estatísticas (sempre double)
make_stats_dt <- function(x) {
  x2 <- x[is.finite(x)]
  n  <- length(x2)
  if (n == 0) {
    return(data.table(N=0.0, Mean=NA_real_, SD=NA_real_, Min=NA_real_, Max=NA_real_))
  }
  data.table(
    N    = as.numeric(n),
    Mean = as.numeric(mean(x2)),
    SD   = as.numeric(if (n > 1) sd(x2) else NA_real_),
    Min  = as.numeric(min(x2)),
    Max  = as.numeric(max(x2))
  )
}

# 2) Long "limpo": Variable x Stat x treated_group
long_group <- rbindlist(lapply(vars_desc, function(v){
  tmp <- panel[, make_stats_dt(get(v)), by = treated_group]
  tmp_long <- melt(tmp,
                   id.vars = "treated_group",
                   variable.name = "Stat",
                   value.name = "value")
  tmp_long[, Variable := v]
  tmp_long[]
}), use.names = TRUE, fill = TRUE)

# Checagem (agora TEM que ser)
unique(long_group$Stat)

# 3) Wide: Control vs Treated
tab_group <- dcast(long_group, Variable + Stat ~ treated_group, value.var = "value")
setnames(tab_group, c("0","1"), c("Control", "Treated"))

# 4) Ordenar + arredondar (1 casa)
stat_order <- c("N","Mean","SD","Min","Max")
tab_group[, Stat := factor(Stat, levels = stat_order)]
setorder(tab_group, Variable, Stat)

tab_group[Stat == "N", c("Control","Treated") := lapply(.SD, as.integer), 
          .SDcols = c("Control","Treated")]
tab_group[Stat != "N", c("Control","Treated") := lapply(.SD, 
          function(x) round(x, 1)), .SDcols = c("Control","Treated")]

# 5) Aplicar labels 
tab_group[, Variable_label := fifelse(Variable %chin% names(var_labels), 
                                      var_labels[Variable], Variable)]

tab_group[, c("Variable") := NULL]



kbl(tab_group[, .(Variable_label, Stat, Control, Treated)],
    format = "latex", booktabs = TRUE, digits = 1,
    caption = "Descriptive statistics by treatment status") %>%
  kable_styling(latex_options = "hold_position", font_size = 10)

#Balance table

vars_balance <- c(
  "share_income1",
  "log_gdp_pc",
  "trade_frac",
  "tax_revenue_frac",
  "gross_fixed_capital_frac",
  "working_age_pop",
  "log_patent"
)


balance_table <- rbindlist(lapply(vars_balance, function(v){
  
  x_treat  <- panel[treated_group == 1, get(v)]
  x_control<- panel[treated_group == 0, get(v)]
  
  mean_treat   <- mean(x_treat, na.rm = TRUE)
  mean_control <- mean(x_control, na.rm = TRUE)
  
  diff <- mean_treat - mean_control
  
  # teste t simples
  pval <- tryCatch(
    t.test(x_treat, x_control)$p.value,
    error = function(e) NA_real_
  )
  
  data.table(
    Variable      = v,
    Mean_Control  = mean_control,
    Mean_Treated  = mean_treat,
    Difference    = diff,
    p_value       = pval
  )
}))


balance_table[, `:=`(
  Mean_Control = round(Mean_Control, 2),
  Mean_Treated = round(Mean_Treated, 2),
  Difference   = round(Difference, 2),
  p_value      = round(p_value, 3)
)]

balance_table[, Variable := var_labels[Variable]]


kbl(
  balance_table,
  format = "latex",
  booktabs = FALSE,
  align = "lrrrr",
  caption = "Balance table: Treated vs Control countries"
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  )


# 9 - graph---------------------------------------------------------------

# 0) grupos


panel[, .N, by = treated_group]
panel[treated_group == 1, uniqueN(Code)]


View(panel[,list(Country, year, Reform.Dummy, treated_group)])


# 1) primeiro ano de reforma (sem warnings)

panel[, first_treat_year :=
        if (any(Reform.Dummy == 1, na.rm=TRUE))
          min(year[Reform.Dummy == 1], na.rm=TRUE)
      else NA_integer_,
      by = Code]


panel[Country=="Australia",
      .(first_treat_year=unique(first_treat_year))]

# 2) indicador pré-tratamento
# pre-period flag
panel[, pre_period := 0L]
panel[treated_group == 1 & !is.na(first_treat_year) & year < first_treat_year, pre_period := 1L]
panel[treated_group == 0, pre_period := 1L]

dt_pre <- panel[pre_period == 1]

View(panel[,list(Country, year, Reform.Dummy, pre_period, treated_group, first_treat_year)])

# 3) restringir never-treated ao mesmo suporte de anos do pré dos tratados
yr_min <- dt_pre[treated_group == 1, min(year, na.rm = TRUE)]
yr_max <- dt_pre[treated_group == 1, max(year, na.rm = TRUE)]

dt_pre <- dt_pre[year >= yr_min & year <= yr_max]

View(dt_pre[,list(Country, year, Reform.Dummy, pre_period, treated_group, first_treat_year)])




# 10 - Package DID---------------------------------------------------------------

panel[, id := .GRP, by = Code]


panel[, gvar := first_treat_year]
panel[is.na(gvar), gvar := 0]


View(panel[,list(Country, year, id, Reform.Dummy, treated_group,first_treat_year, gvar)])


panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[gvar > 0]),
  n_never = uniqueN(id[gvar == 0])
)]

panel[, uniqueN(gvar)]
unique <- sort(unique(panel$gvar))
unique

#type1
#control_group = "notyettreated"
att_gt_obj <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "notyettreated"
)


#Event Studies
es <- aggte(att_gt_obj, type = "dynamic")
summary(es)

ggdid(es)

#outras janelas
es <- aggte(att_gt_obj,
            type = "dynamic",
            min_e = -5,
            max_e = 10)

summary(es)

ggdid(es)



#type2
#control_group = "nevertreated"
att_gt_obj <- att_gt(
  yname = "share_income1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "nevertreated"
)


#Event Studies
es <- aggte(att_gt_obj, type = "dynamic")
summary(es)

ggdid(es)


#check1
setorder(panel, Code, year)

panel[, post_adoption_zero := {
  if (all(is.na(first_treat_year))) FALSE
  else {
    t0 <- first_treat_year[1]
    if (is.na(t0)) FALSE
    else any(Reform.Dummy[year >= t0] == 0, na.rm = TRUE)
  }
}, by = Code]

panel[post_adoption_zero == TRUE, .(Country=unique(Country), first_treat_year=unique(first_treat_year))][]

#check2
panel[, notyet := as.integer(gvar == 0 | year < gvar)]
panel[, .(n_notyet = uniqueN(Code[notyet==1]),
          n_treated_or_post = uniqueN(Code[notyet==0])),
      by = year][order(year)]

#check3
panel[gvar>0, .N, by=gvar][order(gvar)]

#check4
panel[gvar>0, .(n_countries = uniqueN(Code)), by=gvar][order(gvar)]


