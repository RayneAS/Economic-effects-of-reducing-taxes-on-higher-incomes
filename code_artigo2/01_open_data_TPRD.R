rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr",
  "readxl",
  "haven",
  "ggplot2",
  "scales",
  "countrycode"
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
library(countrycode)



# Set user
user = "Rayne"

if (user == "Rayne") {
  data_dir <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/controles"
  data_dir2 <- "C:/Users/Rayne/Documents/2026/projeto_taxacao_desigualdade/dados/TPRD"
  
  working_dir <- "C:/Users/Rayne/Documents/@github/Economic-effects-of-reducing-taxes-on-higher-incomes"
}

code_dir <- file.path(working_dir, "code")
figure_dir <- file.path(working_dir, "output_artigo2")


# 1 - open raw data------------------------------------------------------------

raw_dt <- as.data.table(read_excel(
  file.path(data_dir2, "taxmeasuresdatabase.xlsx"),
  sheet = "TPRD"
))

dt_fig <- copy(raw_dt)

colnames(dt_fig)

class(dt_fig$year_announcement)
dt_fig[, year_announcement := as.numeric(year_announcement)]

table(dt_fig$year_announcement)

  table(dt_fig$TAX_major)
  table(dt_fig$TAX_change)
  table(dt_fig$TAX_type)
  table(dt_fig$TAX_reformtype)

#checagens
  
  # 1. BASE
  n_base_dec <- nrow(unique(
    dt_fig[
      TAX_reformtype == "BASE" & TAX_change == "DEC",
      .(country, year_announcement)
    ]
  ))
  
  
  n_base_inc <- nrow(unique(
    dt_fig[
      TAX_reformtype == "BASE" & TAX_change == "INC",
      .(country, year_announcement)
    ]
  ))
  
  # 2. RATE
  n_rate_dec <- nrow(unique(
    dt_fig[
      TAX_reformtype == "RATE" & TAX_change == "DEC",
      .(country, year_announcement)
    ]
  ))
  
  n_rate_inc <- nrow(unique(
    dt_fig[
      TAX_reformtype == "RATE" & TAX_change == "INC",
      .(country, year_announcement)
    ]
  ))
  
  # 3. BASE country-year
  n_base <- nrow(unique(
    dt_fig[TAX_reformtype == "BASE", .(country, year_announcement)]
  ))
  
  # 4. RATE country-year
  n_rate <- nrow(unique(
    dt_fig[TAX_reformtype == "RATE", .(country, year_announcement)]
  ))
  
  # 5. ALL country-year
  n_all <- nrow(unique(dt_fig[, .(country, year_announcement)]))
  
  # 6. total measures
  n_measures <- nrow(dt_fig)
  
  # 7. average
  avg <- n_measures / n_all
  
  list(
    base_country_year = n_base,
    rate_country_year = n_rate,
    all_country_year = n_all,
    measures = n_measures,
    avg = avg
  )
  
  list(
    base_dec = n_base_dec,
    base_inc = n_base_inc,
    rate_dec = n_rate_dec,
    rate_inc = n_rate_inc
  )
  
#filter data to be like the original paper
dt_fig <- dt_fig[year_announcement>=1990]
dt_fig <- dt_fig[country!="CHN" & country!="IND"]
dt_fig <- dt_fig[TAX_major==1]


# 2 - Create figures -----------------------------------------------------------

# Keep only reform types and directions shown in Figure 2
dt_fig <- dt_fig[
  TAX_reformtype %in% c("BASE", "RATE") &
    TAX_change %in% c("INC", "DEC")
]

# Keep only tax types shown in Figure 2
dt_fig <- dt_fig[TAX_type %in% c("CIT", "PIT", "VAT", "SSC", "EXE", "PRO")]


# Code direction of each raw reform:
# INC = +1, DEC = -1
dt_fig[, a := fifelse(TAX_change == "INC", 1L, -1L)]

#View(dt_fig[,list(country,year_announcement, TAX_change, a, TAX_reformtype)]) 

table(dt_fig$TAX_change)
table(dt_fig$a)

# Aggregate within country-year-tax type-reform type
coded_dt <- dt_fig[
  ,
  .(A = sum(a)),
  by = .(country, year_announcement, TAX_type, TAX_reformtype)
]


#View(coded_dt[,list(country,year_announcement,TAX_reformtype,A)]) 

# Convert the aggregated sign into the coded reform direction
# A > 0  -> inc
# A < 0  -> dec
# A == 0 -> offsetting reforms, dropped from the figure
coded_dt[, dir_coded := fifelse(
  A > 0, "inc",
  fifelse(A < 0, "dec", NA_character_)
)]

coded_dt <- coded_dt[!is.na(dir_coded)]

#View(coded_dt[,list(country,year_announcement,TAX_reformtype,A, dir_coded)]) 


# Create combined x-axis variable
coded_dt[, reform_dir := paste0(
  tolower(TAX_reformtype), "_", dir_coded
)]

# View(coded_dt[,list(country,year_announcement,TAX_reformtype,A, dir_coded, 
#                     reform_dir)]) 

table(coded_dt$reform_dir, useNA = "ifany")

# Order variables
coded_dt[, reform_dir := factor(
  reform_dir,
  levels = c("base_inc", "base_dec", "rate_inc", "rate_dec")
)]

coded_dt[, TAX_type := factor(
  TAX_type,
  levels = c("CIT", "PIT", "VAT", "SSC", "EXE", "PRO")
)]

# Count coded reforms by reform direction and tax type
plot_dt <- coded_dt[, .N, by = .(reform_dir, TAX_type)]

# Create share variable
plot_dt[, share := N / sum(N), by = reform_dir]

# Check final table
plot_dt[order(reform_dir, TAX_type)]

#wide-format check
dcast(plot_dt, TAX_type ~ reform_dir, value.var = "N")

## figure1 ---------------------------------------------------------------------
fig1 <- ggplot(plot_dt, aes(x = reform_dir, y = share, fill = TAX_type)) +
  geom_col(width = 0.4) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(values = c(
    "CIT" = "#4F81BD",
    "PIT" = "#C0504D",
    "VAT" = "#9BBB59",
    "SSC" = "#8064A2",
    "EXE" = "#4BACC6",
    "PRO" = "#F79646"
  )) +
  labs(
    title = "",
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

# (a) Percentage of changes by tax types, reform types and directions
ggsave(
  file.path(figure_dir, "percentage_tax_types_reform_types_directions.jpg"),
  plot = fig1,
  height = 4,
  width = 6
)

## figure2 ---------------------------------------------------------------------
fig2 <- ggplot(plot_dt, aes(x = reform_dir, y = N, fill = TAX_type)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.72) +
  geom_text(
    aes(label = N),
    position = position_dodge(width = 0.82),
    vjust = -0.35,
    size = 2
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
    title = "",
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

# (b) Number of changes by tax types, reform types and directions
ggsave(
  file.path(figure_dir, "number_tax_types_reform_types_directions.jpg"),
  plot = fig2,
  height = 4,
  width = 6
)



## figure3 ---------------------------------------------------------------------

#keep zeros
heat_dt <- dt_fig[
  ,
  .(TC = sum(a)),
  by = .(country, year_announcement, TAX_type, TAX_reformtype)
]

#create collumns: CIT_b, CIT_r etc.
heat_dt[, var := paste0(
  TAX_type, "_",
  ifelse(TAX_reformtype == "BASE", "b", "r")
)]

#aggregate (sum countries)
heat_dt_year <- heat_dt[
  ,
  .(TC = sum(TC)),
  by = .(year_announcement, var)
]

#change to wide
heat_wide <- dcast(
  heat_dt_year,
  year_announcement ~ var,
  value.var = "TC",
  fill = 0
)

#order years
setorder(heat_wide, year_announcement)

heat_wide

heat_long <- melt(
  heat_wide,
  id.vars = "year_announcement",
  variable.name = "tax_var",
  value.name = "TC"
)

heat_long[, tax_var := factor(
  tax_var,
  levels = c(
    "CIT_b", "CIT_r",
    "PIT_b", "PIT_r",
    "VAT_b", "VAT_r",
    "SSC_b", "SSC_r",
    "EXE_b", "EXE_r",
    "PRO_b", "PRO_r"
  )
)]

heat_long[, year_announcement := factor(
  year_announcement,
  levels = sort(unique(year_announcement), decreasing = TRUE)
)]

fig3 <- ggplot(heat_long, aes(x = tax_var, y = year_announcement, fill = TC)) +
  geom_tile(color = "white") +
  geom_text(aes(label = TC), size = 3) +
  
  scale_x_discrete(position = "top") +
  
  scale_fill_gradient2(
    low = "#d6604d",
    mid = "white",
    high = "#1a9850",
    midpoint = 0
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "TC"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 0,      
      hjust = 0.5,
      size = 11
    ),
    
    axis.text.y = element_text(size = 11)
  )

fig3

ggsave(
  file.path(figure_dir, "heatmap.jpg"),
  plot = fig3,
  height = 6,
  width = 10
)

rm(plot_dt, heat_dt_year, heat_long, heat_wide)

