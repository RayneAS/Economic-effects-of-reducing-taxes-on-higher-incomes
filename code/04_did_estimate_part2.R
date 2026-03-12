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
  "ggplot2",
  "fixest",
  "contdid"
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
library(ggplot2)
library(fixest)
library(contdid)


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
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
panel[, year := as.integer(year)]
stopifnot(is.integer(panel$year))

panel <- panel[year >= 1980]

panel[is.na(Reform.Dummy), Reform.Dummy := 0L]
panel[is.na(Significant.Reform), Significant.Reform := 0L]
stopifnot("Reform.Dummy" %in% names(panel))

# 2 - Define auxiliary vars to did package -------------------------------------

setorder(panel, Code, year)

#Define First of treatment (Tax reform)
panel[, first_treat_year :=
        if (any(Reform.Dummy == 1, na.rm=TRUE))
          min(year[Reform.Dummy == 1], na.rm=TRUE)
      else NA_integer_,
      by = Code]

#Define country numeric id did package
panel[, id := .GRP, by = Code]

#Define gvar did package
panel[, gvar := first_treat_year]
panel[is.na(gvar), gvar := 0]

#Define dose as level at adoption
dose_dt <- panel[gvar > 0 & year == gvar, .(dose = -Omega), by = Code]
panel <- merge(panel, dose_dt, by = "Code", all.x = TRUE)

#Define dose=0 for never-treated
panel[gvar == 0, dose := 0]

#dose=0 for treated with Omega missing at adoption (Rubolino-imputed NA)
panel[gvar > 0 & is.na(dose), dose := 0]

#Define continuous adoption: treated only if dose>0
panel[, gvar_cont := fifelse(dose > 0, gvar, 0L)]

panel[gvar_cont == 0, dose := 0]

# 2.1. checks--------------------------------------------------------------------

#check1
panel[, .(
  n_units = uniqueN(id),
  n_treated = uniqueN(id[gvar > 0]),
  n_never = uniqueN(id[gvar == 0])
)]

panel[, uniqueN(gvar)]
unique_g_var <- sort(unique(panel$gvar))
unique_g_var

#check2
check_gvar <- panel[gvar > 0,
                    .(
                      gvar_unique = unique(gvar),
                      min_year_treated = min(year[Reform.Dummy == 1], na.rm = TRUE)
                    ),
                    by = .(Code, Country)
]

check_gvar[gvar_unique != min_year_treated]

#check3
# sanity checks required by contdid (time-invariant)
stopifnot(panel[, all(uniqueN(dose) == 1L), by = Code]$V1 |> all())
stopifnot(panel[, all(uniqueN(gvar_cont) == 1L), by = Code]$V1 |> all())

#check4
#who switches to never-treated in continuous definition
panel[gvar > 0 & gvar_cont == 0,
      .(Country=unique(Country), gvar=unique(gvar), dose=unique(dose)),
      by=Code][]

#check5
#dose constant by country
panel[, .(uniq_dose = uniqueN(dose)), by = Code][uniq_dose != 1]

#check6
#gvar constant by country
panel[, .(uniq_gvar = uniqueN(gvar)), by = Code][uniq_gvar != 1]

#never-treated: gvar==0 e dose time-invariant (0 ou NA, mas constante)
panel[gvar == 0, .(uniq_dose = uniqueN(dose)), by = Code][uniq_dose != 1]


panel[gvar > 0 & is.na(dose), .N, by = .(Code, Country)][order(-N)]


#check7
#View(panel[,list(Country,Code, year, Reform.Dummy, gvar, dose, Omega)])

panel[gvar > 0 & is.na(dose), .N]
panel[gvar == 0 & is.na(dose), .N]

summary(panel$dose)
table(is.na(panel$dose))
table(is.na(panel$pt_share_top1))
table(is.na(panel$log_gdp_pc))
table(is.na(panel$trade_frac))
table(is.na(panel$gross_fixed_capital_frac))
table(is.na(panel$working_age_pop))
table(is.na(panel$tax_revenue_frac))
table(is.na(panel$gross_savings_frac))


# 3- DID continuous estimated with contdid package ---------------------------------

#Treatment is omega intensity 

# 3.1. Income share pre tax------------------------------------------------------

vars_need <- c("Code", "year", "pt_share_top1", "dose", "gvar_cont")
dt_cc <- panel[complete.cases(panel[, ..vars_need])]

dt_cc[, .N, by = Code][, table(N)]

res_cont <- cont_did(
  yname = "pt_share_top1",
  dname = "dose",
  gname = "gvar_cont",
  tname = "year",
  idname = "Code",
  xformula = ~1,
  data = dt_cc,
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 999,
  cband = TRUE,
  num_knots = 2,
  degree = 1,
)

summary(res_cont)

ggcont_did(res_cont)

#checks
names(res_cont)
names(res_cont$event_study)
str(res_cont$event_study, max.level = 1)

#extract estimated values
es <- data.table(
  e   = res_cont$event_study$egt,
  att = res_cont$event_study$att.egt,
  se  = res_cont$event_study$se.egt
)

crit <- unname(res_cont$event_study$crit.val.egt)

es[, `:=`(
  lo = att - crit * se,
  hi = att + crit * se
)]

es_win <- es[e >= -5 & e <= 10]

#plot figure with selected legs and leads

png(file.path(figure_dir, "event_study_dose_share_income1_pretax_contdid.png"),
    width = 1600, height = 1000, res = 200)

ggplot(es_win, aes(x = e, y = att)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) +
  labs(
    x = "Event time",
    y = "ATT"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

dev.off()

#table
tab_es_1 <- es_win[, .(
  e,
  coef = round(att, 4),
  se   = round(se, 4),
  lo   = round(lo, 4),
  hi   = round(hi, 4)
)]

tab_es_1[, outcome := "Income share 1% pre tax"]


#test
pre_idx <- which(es$e >= -5 & es$e <= -1)
beta <- es$att[pre_idx]

IFmat <- res_cont$event_study$inf.function$dynamic.inf.func.e

V <- cov(IFmat[, pre_idx, drop = FALSE])

W <- as.numeric(t(beta) %*% solve(V) %*% beta)
df <- length(beta)
pval <- 1 - pchisq(W, df)

W
pval


# 3.2. Income share post tax------------------------------------------------------

vars_need <- c("Code", "year", "d_share_top1", "dose", "gvar_cont")
dt_cc <- panel[complete.cases(panel[, ..vars_need])]

dt_cc[, .N, by = Code][, table(N)]

res_cont <- cont_did(
  yname = "d_share_top1",
  dname = "dose",
  gname = "gvar_cont",
  tname = "year",
  idname = "Code",
  xformula = ~1,
  data = dt_cc,
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 999,
  cband = TRUE,
  num_knots = 2,
  degree = 1,
)

summary(res_cont)

ggcont_did(res_cont)

#checks
names(res_cont)
names(res_cont$event_study)
str(res_cont$event_study, max.level = 1)

#extract estimated values
es <- data.table(
  e   = res_cont$event_study$egt,
  att = res_cont$event_study$att.egt,
  se  = res_cont$event_study$se.egt
)

crit <- unname(res_cont$event_study$crit.val.egt)

es[, `:=`(
  lo = att - crit * se,
  hi = att + crit * se
)]

es_win <- es[e >= -5 & e <= 10]

#plot figure with selected legs and leads

png(file.path(figure_dir, "event_study_dose_share_income1_posttax_contdid.png"),
    width = 1600, height = 1000, res = 200)

ggplot(es_win, aes(x = e, y = att)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) +
  labs(
    x = "Event time",
    y = "ATT"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

dev.off()

#table
tab_es_2 <- es_win[, .(
  e,
  coef = round(att, 4),
  se   = round(se, 4),
  lo   = round(lo, 4),
  hi   = round(hi, 4)
)]

tab_es_2[, outcome := "Income share 1% post tax"]



#test
pre_idx <- which(es$e >= -5 & es$e <= -1)
beta <- es$att[pre_idx]

IFmat <- res_cont$event_study$inf.function$dynamic.inf.func.e

V <- cov(IFmat[, pre_idx, drop = FALSE])

W <- as.numeric(t(beta) %*% solve(V) %*% beta)
df <- length(beta)
pval <- 1 - pchisq(W, df)

W
pval

# 3.3. Gini pre tax------------------------------------------------------

vars_need <- c("Code", "year", "gini_pre_tax", "dose", "gvar_cont")
dt_cc <- panel[complete.cases(panel[, ..vars_need])]

dt_cc[, .N, by = Code][, table(N)]

res_cont <- cont_did(
  yname = "gini_pre_tax",
  dname = "dose",
  gname = "gvar_cont",
  tname = "year",
  idname = "Code",
  xformula = ~1,
  data = dt_cc,
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 999,
  cband = TRUE,
  num_knots = 2,
  degree = 1,
)

summary(res_cont)

ggcont_did(res_cont)

#checks
names(res_cont)
names(res_cont$event_study)
str(res_cont$event_study, max.level = 1)

#extract estimated values
es <- data.table(
  e   = res_cont$event_study$egt,
  att = res_cont$event_study$att.egt,
  se  = res_cont$event_study$se.egt
)

crit <- unname(res_cont$event_study$crit.val.egt)

es[, `:=`(
  lo = att - crit * se,
  hi = att + crit * se
)]

es_win <- es[e >= -5 & e <= 10]

#plot figure with selected legs and leads

png(file.path(figure_dir, "event_study_dose_gini_pretax_contdid.png"),
    width = 1600, height = 1000, res = 200)

ggplot(es_win, aes(x = e, y = att)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) +
  labs(
    x = "Event time",
    y = "ATT"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

dev.off()

#table
tab_es_3 <- es_win[, .(
  e,
  coef = round(att, 4),
  se   = round(se, 4),
  lo   = round(lo, 4),
  hi   = round(hi, 4)
)]

tab_es_3[, outcome := "Gini pre tax"]


#test
pre_idx <- which(es$e >= -5 & es$e <= -1)
beta <- es$att[pre_idx]

IFmat <- res_cont$event_study$inf.function$dynamic.inf.func.e

V <- cov(IFmat[, pre_idx, drop = FALSE])

W <- as.numeric(t(beta) %*% solve(V) %*% beta)
df <- length(beta)
pval <- 1 - pchisq(W, df)

W
pval


# 3.4. Gini post tax------------------------------------------------------

vars_need <- c("Code", "year", "gini_post_tax", "dose", "gvar_cont")
dt_cc <- panel[complete.cases(panel[, ..vars_need])]

dt_cc[, .N, by = Code][, table(N)]

res_cont <- cont_did(
  yname = "gini_post_tax",
  dname = "dose",
  gname = "gvar_cont",
  tname = "year",
  idname = "Code",
  xformula = ~1,
  data = dt_cc,
  target_parameter = "level",
  aggregation = "eventstudy",
  treatment_type = "continuous",
  control_group = "notyettreated",
  biters = 999,
  cband = TRUE,
  num_knots = 2,
  degree = 1,
)

summary(res_cont)

ggcont_did(res_cont)

#checks
names(res_cont)
names(res_cont$event_study)
str(res_cont$event_study, max.level = 1)

#extract estimated values
es <- data.table(
  e   = res_cont$event_study$egt,
  att = res_cont$event_study$att.egt,
  se  = res_cont$event_study$se.egt
)

crit <- unname(res_cont$event_study$crit.val.egt)

es[, `:=`(
  lo = att - crit * se,
  hi = att + crit * se
)]

es_win <- es[e >= -5 & e <= 10]

#plot figure with selected legs and leads

png(file.path(figure_dir, "event_study_dose_gini_posttax_contdid.png"),
    width = 1600, height = 1000, res = 200)

ggplot(es_win, aes(x = e, y = att)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "gray80", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = 0.5, color = "gray40") +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) +
  labs(
    x = "Event time",
    y = "ATT"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

dev.off()

#table
tab_es_4 <- es_win[, .(
  e,
  coef = round(att, 4),
  se   = round(se, 4),
  lo   = round(lo, 4),
  hi   = round(hi, 4)
)]

tab_es_4[, outcome := "Gini pre tax"]


#test
pre_idx <- which(es$e >= -5 & es$e <= -1)
beta <- es$att[pre_idx]

IFmat <- res_cont$event_study$inf.function$dynamic.inf.func.e

V <- cov(IFmat[, pre_idx, drop = FALSE])

W <- as.numeric(t(beta) %*% solve(V) %*% beta)
df <- length(beta)
pval <- 1 - pchisq(W, df)

W
pval


# 3.5. Produce table and salve results--------------------------------------------


tab_final <- rbind(tab_es_1,tab_es_2) 
tab_final <- rbind(tab_final,tab_es_3) 
tab_final <- rbind(tab_final,tab_es_4) 

#save results
fwrite(tab_final,
       file.path(data_dir, paste0("results_event_study_dose_contdid.csv")),
       sep = ",")





# 4 - TWFE event-study with continuous treatment intensity controls ------------------

#Treatment is omega intensity 

#part2: Treatment is omega intensity and we estimate with 


panel[gvar > 0, e := year - gvar]
panel[gvar == 0, e := 0L] # never-treated não tem event time

#selected leds and lags
dt_es <- panel[(gvar == 0) | (e >= -5 & e <= 10)]

nrow(dt_es[complete.cases(
  pt_share_top1,
  dose,
  log_gdp_pc,
  trade_frac,
  gross_fixed_capital_frac,
  working_age_pop
)])

panel[gvar > 0 & is.na(dose), .N]



#ESTIMATION
# Interações: i(e, dose, ref=-1) cria dummies de e interagidas com dose, 
#omitindo e=-1
m0 <- feols(
  pt_share_top1 ~ i(e, dose, ref = -1) +
    log_gdp_pc + trade_frac + gross_fixed_capital_frac + gross_savings_frac + 
    working_age_pop |
    Code + year,
  data = dt_es, cluster = "Code"
)


summary(m0)

#plot
iplot(m0)

png(file.path(figure_dir, "event_study_dose_share_income1.png"),
    width = 1600, height = 1000, res = 200)

iplot(m0, ref.line = 0,
      xlab = "Event time (e)",
      ylab = "Effect per unit of dose",
      main = "Event study (dose)")

dev.off()




# 6 - Continuous Treatment effect ------------------------------------------------------
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
