rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "knitr",
  "kableExtra",
  "did",
  #"fastglm",
  "ggplot2",
  "fixest"
)


installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(packages, library, character.only = TRUE))


# install.packages("fastglm", type = "source", 
#                  repos = "https://cran.r-project.org")

#PACKAGES USED
library(data.table)
library(readr)
library(knitr)
library(kableExtra)
library(did)
library(ggplot2)
#library(fastglm)
library(fixest)




# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "D:/rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output")


# 1 - open data ----------------------------------------------------------------
panel <- data.table(
  read.csv(
    file.path(data_dir,"complete_dataset.csv" )
  )
)

colnames(panel)

#Add filter because inequality data stats at 1980
#(before that there is a lot of missing values)
panel <- panel[year >= 1980]

panel[is.na(Reform.Dummy), Reform.Dummy := 0L]


# 2 - Define auxiliar vars to did package ------------------------------------------------------

setorder(panel, Code, year)

# ever-treated indicator (country-level)
panel[, treated_group := as.integer(any(Reform.Dummy == 1, na.rm = TRUE)), 
      by = Code]

#View(panel[,list(Country, year, Reform.Dummy, treated_group)])
panel[treated_group == 1, uniqueN(Country)]
panel[, .N, by = treated_group]
panel[treated_group == 1, uniqueN(Code)]

#Define First of treatment (Tax reform)
panel[, first_treat_year :=
        if (any(Reform.Dummy == 1, na.rm=TRUE))
          min(year[Reform.Dummy == 1], na.rm=TRUE)
      else NA_integer_,
      by = Code]


panel[Country=="Australia",
      .(first_treat_year=unique(first_treat_year))]

panel[Country=="Japan",
      .(first_treat_year=unique(first_treat_year))]


# Define pre-period flag
panel[, pre_period := 0L]
panel[treated_group == 1 & !is.na(first_treat_year) & 
        year < first_treat_year, pre_period := 1L]
panel[treated_group == 0, pre_period := 1L]


# View(panel[,list(Country, year, Reform.Dummy, pre_period,
#                  treated_group, first_treat_year)])

#Define country numeric id did package
panel[, id := .GRP, by = Code]

#Define gvar did package
panel[, gvar := first_treat_year]
panel[is.na(gvar), gvar := 0]


# View(panel[,list(Country, year, id, Reform.Dummy,
#                  treated_group,first_treat_year, gvar)])

#data checks
panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[gvar > 0]),
  n_never = uniqueN(id[gvar == 0])
)]

# test <- panel[treated_group==1]
# unique_countri <- unique(test$Country)
# unique_countri

panel[, uniqueN(gvar)]
unique_g_var <- sort(unique(panel$gvar))
unique_g_var

#check g_var
check_gvar <- panel[gvar > 0,
                    .(
                      gvar_unique = unique(gvar),
                      min_year_treated = min(year[Reform.Dummy == 1], na.rm = TRUE)
                    ),
                    by = .(Code, Country)
]

check_gvar[gvar_unique != min_year_treated]

# 3 - Baseline (pre-treatment) summary stats at COUNTRY level------------------------------------------

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
  "gross_fixed_capital_frac",
  "working_age_pop"
)

#define labels
var_labels <- c(
  pt_share_top1           = "Top 1% income share (pre-tax)",
  d_share_top1            = "Top 1% income share (post-tax)",
  gini_pre_tax            = "Gini coefficient (pre-tax income)",
  gini_post_tax           = "Gini coefficient (pos-tax income)",
  Reform.Dummy            = "Tax reform indicator",
  log_gdp_pc              = "Log GDP per capita",
  trade_frac              = "Trade openness",
  tax_revenue_frac        = "Tax revenue",
  gross_fixed_capital_frac= "Gross fixed capital formation",
  working_age_pop         = "Working-age population"
)

missing_vars <- setdiff(vars_baseline, names(panel))
if (length(missing_vars) > 0) {
  stop("Variáveis ausentes em vars_baseline: ", paste(missing_vars, collapse = ", "))
}


#ever-treated: years < gvar
#never-treated: in all years in the sample
panel_pre <- panel[(gvar == 0L) | (year < gvar)]


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


# 4 - DID estimations ------------------------------------------------------

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

panel[post_adoption_zero == TRUE, .(Country=unique(Country), 
                                    first_treat_year=unique(first_treat_year))][]

#check2

panel[, notyet := as.integer(gvar == 0 | year < gvar)]
panel[, .(n_notyet = uniqueN(Code[notyet==1]),
          n_treated_or_post = uniqueN(Code[notyet==0])),
      by = year][order(year)]

#check3

panel[gvar>0, .N, by=gvar][order(gvar)]

#check4

panel[gvar>0, .(n_countries = uniqueN(Code)), by=gvar][order(gvar)]


# 4.1 measure of inequality: pt_share_top1-------------------------------


#Unconditional----------------------------


#Event Studies
# main: notyettreated
att_gt_obj <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "notyettreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -5,
            max_e = 10)

summary(es)

ggdid(es)


p_es <- ggdid(es)

ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)


#Event Studies
# robustness: nevertreated
att_gt_obj <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  data = panel,
  panel = TRUE,
  control_group = "nevertreated"
)

es <- aggte(att_gt_obj, type = "dynamic",
            min_e = -5,
            max_e = 10)

summary(es)

ggdid(es)

p_es <- ggdid(es)

ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated.jpg"), 
       plot = p_es,
       height= 4, width = 6)



#Conditional ----------------------------------

#Event Studies
# main: notyettreated

att_gt_cond <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  xformla = ~ log_gdp_pc + trade_frac +
    gross_fixed_capital_frac + working_age_pop,
  data = panel,
  panel = TRUE,
  control_group = "notyettreated",
  est_method = "reg",
  faster_mode = FALSE
)

es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -5, max_e = 10)
summary(es_cond)

p_cond <- ggdid(es_cond)
p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_notyettreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)



#Event Studies
# robustness: nevertreated

att_gt_cond <- att_gt(
  yname = "pt_share_top1",
  tname = "year",
  idname = "id",
  gname = "gvar",
  xformla = ~ log_gdp_pc + trade_frac +
    gross_fixed_capital_frac + working_age_pop,
  data = panel,
  panel = TRUE,
  control_group = "nevertreated",
  est_method = "reg",
  faster_mode = FALSE
)

es_cond <- aggte(att_gt_cond, type = "dynamic", min_e = -5, max_e = 10)
summary(es_cond)

p_cond <- ggdid(es_cond)
p_cond


ggsave(file.path(figure_dir, "event_study_income_share1_nevertreated_cond.jpg"), 
       plot = p_cond,
       height= 4, width = 6)




# 5 - Continuous Treatment effect ------------------------------------------------------
# Treatment intensity: dose = -Omega at adoption (year==gvar)

# dose by year of adoption of tax reform (by country)
dose_dt <- panel[gvar > 0 & year == gvar, .(dose = -Omega), by = Code]
panel <- merge(panel, dose_dt, by = "Code", all.x = TRUE)
panel[gvar == 0, dose := 0]

# Event time (keep never-treated with e=0 )
panel[gvar > 0, e := year - gvar]
panel[gvar == 0, e := 0L]

# Keep event window + never-treated
dt_es <- panel[(gvar == 0) | (e >= -5 & e <= 10)]

# Outcomes to run
outcomes <- c("pt_share_top1", "d_share_top1", "gini_pre_tax", "gini_post_tax")

# Controls 
controls <- c("log_gdp_pc", "trade_frac", "gross_fixed_capital_frac", "working_age_pop")

# Windows for averages
# pre window excludes ref = -1 by construction; use [-5,-2] to test pre-trends
win_pre   <- -5:-2
win_post0 <- 0:2
win_post1 <- 3:6
win_post2 <- 7:10
win_postA <- 0:10

# Helper: extract event-time coefficients and compute window averages + joint tests
extract_windows <- function(m, win_pre, win_post0, win_post1, win_post2, win_postA) {
  
  b  <- coef(m)
  V  <- vcov(m)           # with cluster="Code")
  cn <- names(b)
  
  # pega somente os coeficientes do event-time x dose
  idx <- grepl("dose", cn) & grepl("e::", cn)
  if (!any(idx)) return(NULL)
  
  b_ev  <- b[idx]
  cn_ev <- cn[idx]
  
  # extrai k do event time a partir do nome (robusto)
  e_k <- suppressWarnings(as.integer(sub(".*e::(-?\\d+).*", "\\1", cn_ev)))
  ok  <- is.finite(e_k)
  b_ev <- b_ev[ok]; cn_ev <- cn_ev[ok]; e_k <- e_k[ok]
  if (length(b_ev) == 0) return(NULL)
  
  mean_win <- function(w){
    sel <- e_k %in% w
    if (!any(sel)) return(NA_real_)
    mean(b_ev[sel], na.rm = TRUE)
  }
  
  # Wald test robusto: H0: coeficientes na janela = 0
  p_wald <- function(w){
    sel <- e_k %in% w
    if (!any(sel)) return(NA_real_)
    
    terms <- cn_ev[sel]
    pos   <- match(terms, cn)     # posições no vetor completo de coeficientes
    
    pos <- pos[is.finite(pos)]
    if (length(pos) == 0) return(NA_real_)
    
    bS <- as.numeric(b[pos])
    VS <- V[pos, pos, drop = FALSE]
    
    # Se VS for singular, usa pseudo-inversa
    invVS <- tryCatch(solve(VS), error = function(e) NULL)
    if (is.null(invVS)) {
      if (!requireNamespace("MASS", quietly = TRUE)) return(NA_real_)
      invVS <- MASS::ginv(VS)
    }
    
    W <- drop(t(bS) %*% invVS %*% bS)  # estatística Wald ~ Chi^2(q)
    q <- length(bS)
    
    pchisq(W, df = q, lower.tail = FALSE)
  }
  
  list(
    avg_pre    = mean_win(win_pre),
    avg_post0  = mean_win(win_post0),
    avg_post1  = mean_win(win_post1),
    avg_post2  = mean_win(win_post2),
    avg_postA  = mean_win(win_postA),
    p_pre      = p_wald(win_pre),
    p_postA    = p_wald(win_postA)
  )
}

# Store models + table results
models_dose <- list()
tab_windows <- data.table()

for (y in outcomes) {
  
  message("Running dose event-study for: ", y)
  
  # outcome-specific complete-case sample
  vars_need <- c(y, "dose", controls, "e", "Code", "year", "gvar")
  dt_cc <- dt_es[complete.cases(dt_es[, ..vars_need])]
  
  diag <- dt_cc[, .(
    n = .N,
    n_treated = sum(gvar > 0),
    n_treated_nonzero_dose = sum(gvar > 0 & dose != 0),
    dose_sd = sd(dose),
    e_unique = uniqueN(e)
  )]
  print(diag)
  
  if (nrow(dt_cc) == 0 || diag$n_treated == 0 || diag$n_treated_nonzero_dose == 0) {
    warning("Skipping ", y, " (insufficient data after filtering).")
    next
  }
  
  # estimate
  fml <- as.formula(paste0(
    y, " ~ i(e, dose, ref = -1) + ",
    paste(controls, collapse = " + "),
    " | Code + year"
  ))
  
  m <- feols(fml, data = dt_cc, cluster = "Code")
  models_dose[[y]] <- m
  
  # plot
  out_png <- file.path(figure_dir, paste0("event_study_dose_", y, ".png"))
  png(out_png, width = 1600, height = 1000, res = 200)
  iplot(
    m,
    ref.line = 0,
    xlab = "Event time (e)",
    ylab = "Effect per unit of dose",
    main = paste0("Dose event-study: ", y)
  )
  dev.off()
  
  # window averages + joint tests
  w <- extract_windows(m, win_pre, win_post0, win_post1, win_post2, win_postA)
  if (is.null(w)) {
    warning("Could not find event-time x dose coefficients for: ", y)
    next
  }
  
  tab_windows <- rbind(
    tab_windows,
    data.table(
      outcome   = y,
      N         = nobs(m),
      avg_pre   = w$avg_pre,
      avg_0_2   = w$avg_post0,
      avg_3_6   = w$avg_post1,
      avg_7_10  = w$avg_post2,
      avg_0_10  = w$avg_postA,
      p_pre     = w$p_pre,
      p_post    = w$p_postA
    ),
    fill = TRUE
  )
}

# ---------- Export .tex: window-averages table (main result) ---------------------

# labels
outcome_labels <- c(
  pt_share_top1 = "Top 1% income share (pre-tax)",
  d_share_top1  = "Top 1% income share (post-tax)",
  gini_pre_tax  = "Gini (pre-tax)",
  gini_post_tax = "Gini (post-tax)"
)

tab_windows[, outcome := fifelse(outcome %chin% names(outcome_labels),
                                 outcome_labels[outcome], outcome)]

# rounding
tab_windows[, `:=`(
  avg_pre  = round(avg_pre, 2),
  avg_0_2  = round(avg_0_2, 2),
  avg_3_6  = round(avg_3_6, 2),
  avg_7_10 = round(avg_7_10, 2),
  avg_0_10 = round(avg_0_10, 2),
  p_pre    = round(p_pre, 2),
  p_post   = round(p_post, 2)
)]

tex_out <- file.path(figure_dir, "etable_dose_eventstudy_windows.tex")

# Use knitr + kableExtra to make a clean LaTeX table
kbl(
  tab_windows[, .(Outcome = outcome, N,
                  `Avg pre (-5,-2)` = avg_pre,
                  `Avg (0,2)` = avg_0_2,
                  `Avg (3,6)` = avg_3_6,
                  `Avg (7,10)` = avg_7_10,
                  `Avg post (0,10)` = avg_0_10,
                  `Pre-trend p-val` = p_pre,
                  `Post (0,10) p-val` = p_post)],
  format = "latex",
  booktabs = TRUE,
  align = "lrrrrrrrr",
  caption = "Dose event-study: window-averaged effects and joint tests",
  label = "tab:dose_eventstudy_windows"
) |>
  kable_styling(latex_options = "hold_position", font_size = 10) |>
  save_kable(file = tex_out)

message("Saved LaTeX table: ", tex_out)