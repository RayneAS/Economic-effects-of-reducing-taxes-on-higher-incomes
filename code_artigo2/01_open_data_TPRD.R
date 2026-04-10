rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "haven",
  "ggplot2",
  "scales"
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
library(readxl)
library(haven)
library(ggplot2)
library(scales)


# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/TPRD"
  
  working_dir <- "D:/rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")

# 1 - open raw data------------------------------------------------------------

raw_dt <- as.data.table(read_excel(
  file.path(data_dir2, "taxmeasuresdatabase.xlsx"),
  sheet = "TPRD"
))

dt_fig <- copy(raw_dt)

colnames(dt_fig)

class(dt_fig$year_announcement)
dt_fig[, year_announcement := as.numeric(year_announcement)]

#filter data to be like the original paper
dt_fig <- dt_fig[year_announcement>=1990]
dt_fig <- dt_fig[country!="CHN" & country!="IND"]
dt_fig <- dt_fig[TAX_major==1]


# 2 - Create figures------------------------------------------------------------

#filter types from the figure
dt_fig <- dt_fig[
  TAX_reformtype %in% c("BASE", "RATE") &
    TAX_change %in% c("INC", "DEC")
]

#create var to reform 
dt_fig[, reform_dir := paste0(tolower(TAX_reformtype), 
                              "_", tolower(TAX_change))]
table(dt_fig$reform_dir)

#order data
dt_fig[, reform_dir := factor(
  reform_dir,
  levels = c("base_inc", "base_dec", "rate_inc", "rate_dec")
)]

# keep only few types of tax
dt_fig <- dt_fig[TAX_type %in% c("CIT", "PIT", "VAT", "SSC", "EXE", "PRO")]

dt_fig[, TAX_type := factor(
  TAX_type,
  levels = c("CIT", "PIT", "VAT", "SSC", "EXE", "PRO")
)]

# count reforms by reform direction and tax type
plot_dt <- dt_fig[, .N, by = .(reform_dir, TAX_type)]

#create share variable
plot_dt[, share := N / sum(N), by = reform_dir]

#check
plot_dt[order(reform_dir, TAX_type)]


#figure1
ggplot(plot_dt, aes(x = reform_dir, y = share, fill = TAX_type)) +
  geom_col(width = 0.4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "CIT" = "#4F81BD",
    "PIT" = "#C0504D",
    "VAT" = "#9BBB59",
    "SSC" = "#8064A2",
    "EXE" = "#4BACC6",
    "PRO" = "#F79646"
  )) +
  labs(
    title = "(a) Percentage of changes by tax types, reform types and directions",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

#figure2
ggplot(plot_dt, aes(x = reform_dir, y = N, fill = TAX_type)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.72) +
  geom_text(
    aes(label = N),
    position = position_dodge(width = 0.82),
    vjust = -0.35,
    size = 4.5
  ) +
  scale_fill_manual(values = c(
    "CIT" = "#4F81BD",
    "PIT" = "#C0504D",
    "VAT" = "#9BBB59",
    "SSC" = "#8064A2",
    "EXE" = "#4BACC6",
    "PRO" = "#F79646"
  )) +
  scale_y_continuous(
    breaks = seq(0, max(plot_dt$N) + 50, by = 50),
    limits = c(0, max(plot_dt$N) * 1.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "(b) Number of changes by tax types, reform types and directions",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
