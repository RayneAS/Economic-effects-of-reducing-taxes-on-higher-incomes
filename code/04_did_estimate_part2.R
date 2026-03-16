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
  results_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/resultados/part_1"
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

tab_es_4[, outcome := "Gini post tax"]


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
       file.path(results_dir, paste0("results_event_study_dose_contdid.csv")),
       sep = ",")

#save results
fwrite(tab_final,
       file.path(results_dir, paste0("results_event_study_dose_contdid.xlsx")),
       sep = ",")

#create table with some results

e_keep <- c(-5, -1, 0, 1, 5, 10)

tab_sel <- copy(tab_final[e %in% e_keep])

#rename for table 
tab_sel[, outcome := fcase(
  outcome == "Income share 1% pre tax",  "Top 1\\% share pre-tax",
  outcome == "Income share 1% post tax", "Top 1\\% share post-tax",
  outcome == "Gini pre tax",             "Gini pre-tax",
  outcome == "Gini post tax",            "Gini post-tax",
  default = outcome
)]

#check
tab_sel[, .N, by = .(e, outcome)][order(outcome, e)]

#select order of columns
tab_sel[, outcome := factor(
  outcome,
  levels = c(
    "Top 1\\% share pre-tax",
    "Top 1\\% share post-tax",
    "Gini pre-tax",
    "Gini post-tax"
  )
)]

#coef + se
tab_sel[, cell := paste0(
  sprintf("%.4f", coef),
  " (", sprintf("%.4f", se), ")"
)]

# wide
tab_wide <- dcast(
  tab_sel,
  e ~ outcome,
  value.var = "cell"
)

setorder(tab_wide, e)

tab_wide

#table
latex_tab <- kbl(
  tab_wide,
  format = "latex",
  booktabs = TRUE,
  align = c("r", "l", "l", "l", "l"),
  caption = "Selected event-study estimates by outcome",
  label = "event_study_compact",
  escape = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size = 9
  )

writeLines(
  as.character(latex_tab),
  file.path(data_dir, "tab_event_study_compact.tex")
)


latex_tab
