rm(list = ls())
gc()

#Install packages
packages <- c(
  "data.table",
  "readr"
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

#List oecd countries

oecd_countries <- c(
  "Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica",
  "Czechia","Denmark","Estonia","Finland","France","Germany","Greece",
  "Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia",
  "Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway",
  "Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden",
  "Switzerland","Türkiye","United Kingdom","United States"
)


# 1 - open gdp data------------------------------------------------------------------
gdp_pc <- data.table(
  read_csv(
    file.path(data_dir, "gdp_per_capita.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

head(gdp_pc)
colnames(gdp_pc)

setnames(gdp_pc, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "GDPpercapita"))

gdp_pc[, c("Indicator Code", "...70") := NULL]


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

rm(gdp_pc)

# 2 - open trade data ---------------------------------------------------------------
trade <- data.table(
  read_csv(
    file.path(data_dir, "trade.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

head(trade)

setnames(trade, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "trade_to_gdp"))


trade[, c("Indicator Code", "...70") := NULL]


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

rm(trade)

# 3 - open tax revenue data----------------------------------------------------------
tax_revenue <- data.table(
  read_csv(
    file.path(data_dir, "tax_revenue.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

head(tax_revenue)

setnames(tax_revenue, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "tax_revenue_to_gdp"))

tax_revenue[, c("Indicator Code", "...70") := NULL]


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

rm(tax_revenue)

# 4 - open gross domestic savings data --------------------------------------------------------------
gross_savings <- data.table(
  read_csv(
    file.path(data_dir, "gross_dom_savings.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

head(gross_savings)

setnames(gross_savings, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "gross_savings"))

gross_savings <- gross_savings[, c("Indicator Code", "...70") := NULL]

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

rm(gross_savings)

# 5 - open gross fixed capital data ---------------------------------------------
gross_fixed_capital <- data.table(
  read_csv(
    file.path(data_dir, "gross_fixed_capital.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

head(gross_fixed_capital)

setnames(gross_fixed_capital, c("Country Name", "Country Code", "Indicator Name"), 
         c("Country", "Code", "gross_fixed_capital"))

gross_fixed_capital <- gross_fixed_capital[, 
                                      c("Indicator Code", "...70") := NULL]


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

rm(gross_fixed_capital)

# 6 - open Bank Deposits data --------------------------------------------------
bank_deposits_to_gdp <- data.table(
  read_csv(
    file.path(data_dir, "bank_deposits_to_gdp.csv"),
    locale = locale(encoding = "UTF-8"),
    na = c("", "NA", "N/A", "..")
  )
)


head(bank_deposits_to_gdp)

setnames(bank_deposits_to_gdp, c("Country Name", "Country Code", "Series Name"), 
         c("Country", "Code", "bank_deposits_to_gdp"))

bank_deposits_to_gdp <- bank_deposits_to_gdp[, ("Series Code") := NULL  ]


#clean year columns
old_names <- names(bank_deposits_to_gdp)

year_cols <- grep("^\\d{4} \\[YR\\d{4}\\]$", old_names, value = TRUE)

setnames(bank_deposits_to_gdp, year_cols, sub("^([0-9]{4}).*$", "\\1", year_cols))

year_cols <- grep("^[0-9]{4}$", names(bank_deposits_to_gdp), value = TRUE)


# reshape to long
bank_deposits_to_gdp_long <- melt(
  bank_deposits_to_gdp,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "bank_deposits_to_gdp",
  variable.factor = FALSE
)


bank_deposits_to_gdp_long <- as.data.table(bank_deposits_to_gdp_long)
bank_deposits_to_gdp_long <- bank_deposits_to_gdp_long[!is.na(Country)]

head(bank_deposits_to_gdp_long)

setorder(bank_deposits_to_gdp_long, Country, year)

rm(bank_deposits_to_gdp)


bank_deposits_to_gdp_long <- bank_deposits_to_gdp_long[,
                                      bank_deposits_to_gdp:= 
                                      as.numeric(bank_deposits_to_gdp)/100]

# 7 - open trade union_density countries----------------------------------------
union_density <- data.table(
  read_csv(
    file.path(data_dir, "union_density.csv"),
    na = c("", "NA", "N/A", "..")))

head(union_density)

setnames(union_density, c("Reference area", "REF_AREA", "OBS_VALUE", 
                          "TIME_PERIOD"), 
         c("Country", "Code", "trade_union","year"))

union_density <- union_density[, .SD,
                               .SDcols = c("Country", "Code", "trade_union",
                                           "year")]



setorder(union_density, Country, year)


# 8 - open working age population data------------------------------------------
working_age_pop <- data.table(
  read_csv(
    file.path(data_dir, "working_age_pop.csv"), na = c("", "NA", "N/A", "..")))


head(working_age_pop)

setnames(working_age_pop, c("Reference area", "REF_AREA", "OBS_VALUE", 
                            "TIME_PERIOD"), 
         c("Country", "Code", "working_age_pop","year"))

working_age_pop <- working_age_pop[, .SD,
                    .SDcols = c("Country", "Code", "working_age_pop","year")]



setorder(working_age_pop, Country, year)

#create oecd dummy
working_age_pop <- working_age_pop[, 
                              oecd:= as.integer(Country %in% oecd_countries)]

#drop years
years_out <- 1950:1959
years_out

working_age_pop <- working_age_pop[!year %in% years_out]

# 9 - open patent data -------------------------------------------------------------

patent <- data.table(
  read_csv(
    file.path(data_dir, "patents.csv"), na = c("", "NA", "N/A", "..")))

head(patent)

setnames(patent, c("Reference area", "REF_AREA", "OBS_VALUE", "TIME_PERIOD"), 
         c("Country", "Code", "patent","year"))

patent <- patent[, .SD,
                 .SDcols = c("Country", "Code", "patent","year")]

setorder(patent, Country, year)


# 10 - Open Stock capitalization data ------------------------------------------------
stocks_capt <- data.table(
  read_csv(
    file.path(data_dir, "stocks_capitalization_to_gdp.csv"),
    na = c("", "NA", "N/A", "..")
    )
)

head(stocks_capt)

setnames(stocks_capt, c("Country Name", "Country Code"), 
         c("Country", "Code"))

#clean year columns
old_names <- names(stocks_capt)
year_cols <- grep("^\\d{4} \\[YR\\d{4}\\]$", old_names, value = TRUE)

setnames(stocks_capt, year_cols, sub("^([0-9]{4}).*$", "\\1", year_cols))

year_cols <- grep("^[0-9]{4}$", names(stocks_capt), value = TRUE)

colnames(stocks_capt)

stocks_capt[, c("Series Name", "Series Code") := NULL]


# reshape to long
stocks_capt_long <- melt(
  stocks_capt,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "stocks_capt",
  variable.factor = FALSE
)

head(stocks_capt_long)

setorder(stocks_capt_long, Country, year)

rm(stocks_capt)

# 11 - Open Stock Trade data ------------------------------------------------
stocks_trade <- data.table(
  read_csv(
    file.path(data_dir, "stocks_trade_to_gdp.csv"),
    skip = 3,
    na = c("", "NA", "N/A", "..")
  )
)

colnames(stocks_trade)

setnames(stocks_trade, c("Country Name", "Country Code"), 
         c("Country", "Code"))


stocks_trade[, c("Indicator Name", "Indicator Code","...70") := NULL]


year_cols <- grep("^[0-9]{4}$", names(stocks_trade), value = TRUE)

#reshape to long
stocks_trade_long <- melt(
  stocks_trade,
  id.vars = c("Country", "Code"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name = "stocks_trade",
  variable.factor = FALSE
)


setorder(stocks_trade_long, Country, year)
head(stocks_trade_long)

rm(stocks_trade)


# 12 - Open Government Gross Debt data ------------------------------------------------
gov_gross_debt <- data.table(
  read_csv(
    file.path(data_dir, "government_gross_debt.csv"),
    na = c("", "NA", "N/A", "..")
  )
)

colnames(gov_gross_debt)


setnames(gov_gross_debt, c("Reference area", "REF_AREA","TIME_PERIOD", "OBS_VALUE"), 
         c("Country", "Code", "year", "gov_gross_debt"))

gov_gross_debt <- gov_gross_debt[, .SD,
                      .SDcols = c("Country", "Code", "year", "gov_gross_debt")]


head(gov_gross_debt)

setorder(gov_gross_debt, Country, year)


#Merge databases --------------------------------------------------------------
panel_data <- Reduce(
  function(x, y) merge(x, y, by = c("Country", "Code", "year"), all = TRUE),
  list(
    gdp_long,
    trade_long,
    tax_revenue_long,
    gross_savings_long,
    gross_fixed_capital_long,
    bank_deposits_to_gdp_long,
    union_density,
    working_age_pop,
    patent,
    stocks_capt_long,
    stocks_trade_long,
    gov_gross_debt
  )
)

#remove old data
rm(gdp_long,
  trade_long,
  tax_revenue_long,
  gross_savings_long,
  gross_fixed_capital_long,
  bank_deposits_to_gdp_long,
  union_density,
  working_age_pop,
  patent,
  stocks_capt_long,
  stocks_trade_long,
  gov_gross_debt)


class(panel_data)

panel_data <- as.data.table(panel_data) 
panel_data[, oecd := fifelse(is.na(oecd), 0L, as.integer(oecd))]

setorder(panel_data, Country, year)

View(panel_data[, list(Country, year, oecd)])


#define panel with oecd countries
panel_oecd <- panel_data[oecd == 1]

unique <-unique(panel_oecd$year) 
unique

unique <-unique(panel_oecd$Country)
unique

#Database checks----------------------------------------------------------------

panel_oecd[, .N, by=.(Country, Code, year)][N>1]

class(panel_oecd$year)
panel_oecd[is.na(year)]

panel_oecd[, uniqueN(oecd)]
panel_oecd[, uniqueN(Code)]
panel_oecd[, sort(unique(Country))]

