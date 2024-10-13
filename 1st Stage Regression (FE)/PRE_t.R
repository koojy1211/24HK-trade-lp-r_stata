# 2024 HK Essay



# Setting Working Directory
setwd("/Users/koojy/Desktop/STUDY/학회/2024 다산금융반 35기/방중 논문/통계 분석/IV-1S")

# Load Libralies
library(readxl)
library(dplyr)
library(tidyr)




#####################################################
# TRADE (Explained Variable for the Relevance Test) #
#####################################################

TRADE = read_xlsx("TRADE.xlsx")

# Filter relevant data for imports and exports as a share of GDP
imports_df <- TRADE %>% filter(`Series Name` == "Imports of goods and services (% of GDP)")
exports_df <- TRADE %>% filter(`Series Name` == "Exports of goods and services (% of GDP)")

# Set Country Name as index to align imports and exports
imports_df <- imports_df %>% select(-`Series Name`, -`Series Code`, -`Country Code`)
exports_df <- exports_df %>% select(-`Series Name`, -`Series Code`, -`Country Code`)

# Convert columns to numeric
imports_df[,-1] <- lapply(imports_df[,-1], as.numeric)
exports_df[,-1] <- lapply(exports_df[,-1], as.numeric)

# Align the imports and exports data frames
aligned_df <- imports_df
aligned_df[,-1] <- imports_df[,-1] + exports_df[,-1]

# Rename columns for clarity
colnames(aligned_df) <- c("country_i", as.character(1980:2023))

# Melt the DataFrame to the desired long format
trade_df <- aligned_df %>%
  pivot_longer(cols = -country_i, names_to = "year", values_to = "TRADE") %>%
  mutate(year = as.integer(year))

trade_df <- trade_df %>%
  arrange(country_i)

unique(trade_df$country_i)
replace_trade <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
trade_df$country_i <- str_replace_all(trade_df$country_i, replace_trade)




##############################
# Multilateral Openness (IV) #
##############################

IV <- read.csv("DATA_IV.csv")

# multilateral openness 계산
opns <- IV %>%
  group_by(country_i_num, year) %>%
  summarise(multilateral_openness = sum(omega_hat, na.rm = TRUE))

colnames(opns) <- c("country_i", "year", "OPEN")
unique(opns$country_i)






#######################################################################
# KOF Globalisation Indices (Control Variable for the Relevance Test) #
#######################################################################

#install.packages("kofdata")
library(httr) ; library(kofdata)
#install.packages("timeseriesdb")
library(timeseriesdb)
keys <- c(
  "ch.kof.globidx.v2020.pogi.ven", "ch.kof.globidx.v2020.pogi.usa",
  "ch.kof.globidx.v2020.pogi.gbr", "ch.kof.globidx.v2020.pogi.tha",
  "ch.kof.globidx.v2020.pogi.esp", "ch.kof.globidx.v2020.pogi.zaf",
  "ch.kof.globidx.v2020.pogi.sgp", "ch.kof.globidx.v2020.pogi.prt",
  "ch.kof.globidx.v2020.pogi.phl", "ch.kof.globidx.v2020.pogi.per",
  "ch.kof.globidx.v2020.pogi.pak", "ch.kof.globidx.v2020.pogi.swe",
  "ch.kof.globidx.v2020.pogi.tur", "ch.kof.globidx.v2020.pogi.nor",
  "ch.kof.globidx.v2020.pogi.nzl", "ch.kof.globidx.v2020.pogi.nld",
  "ch.kof.globidx.v2020.pogi.mex", "ch.kof.globidx.v2020.pogi.mys",
  "ch.kof.globidx.v2020.pogi.kor", "ch.kof.globidx.v2020.pogi.jpn",
  "ch.kof.globidx.v2020.pogi.ita", "ch.kof.globidx.v2020.pogi.irl",
  "ch.kof.globidx.v2020.pogi.idn", "ch.kof.globidx.v2020.pogi.ind",
  "ch.kof.globidx.v2020.pogi.hkg", "ch.kof.globidx.v2020.pogi.grc",
  "ch.kof.globidx.v2020.pogi.fra", "ch.kof.globidx.v2020.pogi.fin",
  "ch.kof.globidx.v2020.pogi.dnk", "ch.kof.globidx.v2020.pogi.deu",
  "ch.kof.globidx.v2020.pogi.col", "ch.kof.globidx.v2020.pogi.chn",
  "ch.kof.globidx.v2020.pogi.chl", "ch.kof.globidx.v2020.pogi.che",
  "ch.kof.globidx.v2020.pogi.can", "ch.kof.globidx.v2020.pogi.bra",
  "ch.kof.globidx.v2020.pogi.bel", "ch.kof.globidx.v2020.pogi.aut",
  "ch.kof.globidx.v2020.pogi.aus", "ch.kof.globidx.v2020.pogi.arg")
counti_vec <- c(
  "Venezuela, RB", "United States", "United Kingdom", "Thailand", "Spain",
  "South Africa", "Singapore", "Portugal", "Philippines", "Peru", "Pakistan",
  "Sweden", "Turkiye", "Norway", "New Zealand", "Netherlands", "Mexico",
  "Malaysia", "Korea, Rep.", "Japan", "Italy", "Ireland", "Indonesia", "India",
  "Hong Kong SAR, China", "Greece", "France", "Finland", "Denmark", "Germany",
  "Colombia", "China", "Chile", "Switzerland", "Canada", "Brazil", "Belgium",
  "Austria", "Australia", "Argentina"
)
KOF_pol = as.data.frame(get_time_series(keys))
keys2 <- c(
  "ch.kof.globidx.v2020.sogi.ven", "ch.kof.globidx.v2020.sogi.usa",
  "ch.kof.globidx.v2020.sogi.gbr", "ch.kof.globidx.v2020.sogi.tha",
  "ch.kof.globidx.v2020.sogi.esp", "ch.kof.globidx.v2020.sogi.zaf",
  "ch.kof.globidx.v2020.sogi.sgp", "ch.kof.globidx.v2020.sogi.prt",
  "ch.kof.globidx.v2020.sogi.phl", "ch.kof.globidx.v2020.sogi.per",
  "ch.kof.globidx.v2020.sogi.pak", "ch.kof.globidx.v2020.sogi.swe",
  "ch.kof.globidx.v2020.sogi.tur", "ch.kof.globidx.v2020.sogi.nor",
  "ch.kof.globidx.v2020.sogi.nzl", "ch.kof.globidx.v2020.sogi.nld",
  "ch.kof.globidx.v2020.sogi.mex", "ch.kof.globidx.v2020.sogi.mys",
  "ch.kof.globidx.v2020.sogi.kor", "ch.kof.globidx.v2020.sogi.jpn",
  "ch.kof.globidx.v2020.sogi.ita", "ch.kof.globidx.v2020.sogi.irl",
  "ch.kof.globidx.v2020.sogi.idn", "ch.kof.globidx.v2020.sogi.ind",
  "ch.kof.globidx.v2020.sogi.hkg", "ch.kof.globidx.v2020.sogi.grc",
  "ch.kof.globidx.v2020.sogi.fra", "ch.kof.globidx.v2020.sogi.fin",
  "ch.kof.globidx.v2020.sogi.dnk", "ch.kof.globidx.v2020.sogi.deu",
  "ch.kof.globidx.v2020.sogi.col", "ch.kof.globidx.v2020.sogi.chn",
  "ch.kof.globidx.v2020.sogi.chl", "ch.kof.globidx.v2020.sogi.che",
  "ch.kof.globidx.v2020.sogi.can", "ch.kof.globidx.v2020.sogi.bra",
  "ch.kof.globidx.v2020.sogi.bel", "ch.kof.globidx.v2020.sogi.aut",
  "ch.kof.globidx.v2020.sogi.aus", "ch.kof.globidx.v2020.sogi.arg")
KOF_soci = as.data.frame(get_time_series(keys2))

colnames(KOF_pol) <- counti_vec
colnames(KOF_soci) <- counti_vec

date_seq <- seq(from = 1970, to = 2021, by = 1)
KOF_pol$DATE <- date_seq
KOF_soci$DATE <- date_seq

counti <- sort(counti_vec)
KOF_pol <- KOF_pol[, c("DATE", counti)]
KOF_soci <- KOF_soci[, c("DATE", counti)]

library(reshape2)
KOF_pol_long <- melt(KOF_pol, id.vars = "DATE", variable.name = "country_i",
                     value.name = "KOF_pol")
colnames(KOF_pol_long)[1] <- "year"
KOF_soci_long <- melt(KOF_soci, id.vars = "DATE", variable.name = "country_i",
                      value.name = "KOF_soci")
colnames(KOF_soci_long)[1] <- "year"

KOF <- merge(KOF_pol_long, KOF_soci_long, by = c("year", "country_i"), all = TRUE)

unique(KOF$country_i)
replace_kof <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
KOF$country_i <- str_replace_all(KOF$country_i, replace_kof)





###########################
# Other Control Variables #
###########################

CONT = read_xlsx("CONTROL.xlsx")

# Extract relevant data for GDP per capita, Population, and Age dependency ratio
gdp_pc_df <- CONT %>% filter(`Series Name` == "GDP per capita (constant 2015 US$)")
pop_df <- CONT %>% filter(`Series Name` == "Population, total")
age_dep_df <- CONT %>% filter(`Series Name` == "Age dependency ratio (% of working-age population)")

# Set Country Name as index to align data
gdp_pc_df <- gdp_pc_df %>% select(-`Series Name`, -`Series Code`, -`Country Code`)
pop_df <- pop_df %>% select(-`Series Name`, -`Series Code`, -`Country Code`)
age_dep_df <- age_dep_df %>% select(-`Series Name`, -`Series Code`, -`Country Code`)

# Convert columns to numeric
gdp_pc_df[,-1] <- lapply(gdp_pc_df[,-1], as.numeric)
pop_df[,-1] <- lapply(pop_df[,-1], as.numeric)
age_dep_df[,-1] <- lapply(age_dep_df[,-1], as.numeric)

# Rename columns for clarity
colnames(gdp_pc_df) <- c("country_i", as.character(1980:2023))
colnames(pop_df) <- c("country_i", as.character(1980:2023))
colnames(age_dep_df) <- c("country_i", as.character(1980:2023))

# Melt the DataFrame to the desired long format
gdp_pc_long <- gdp_pc_df %>%
  pivot_longer(cols = -country_i, names_to = "year", values_to = "GDPpc") %>%
  mutate(year = as.integer(year))

pop_long <- pop_df %>%
  pivot_longer(cols = -country_i, names_to = "year", values_to = "POP") %>%
  mutate(year = as.integer(year))
pop_long$lnPOP <- log(pop_long$POP)

age_dep_long <- age_dep_df %>%
  pivot_longer(cols = -country_i, names_to = "year", values_to = "AGE_dep") %>%
  mutate(year = as.integer(year))

# Merge the long data frames by country and year
merge <- gdp_pc_long %>%
  inner_join(pop_long, by = c("country_i", "year")) %>%
  inner_join(age_dep_long, by = c("country_i", "year"))

# Rename columns for clarity and arrange
CONTROL <- merge %>%
  arrange(country_i, year)

unique(CONTROL$country_i)
replace_CONTROL <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
CONTROL$country_i <- str_replace_all(CONTROL$country_i, replace_CONTROL)





#######################################################
# FDI Index (Control Variable for the Relevance Test) #
#######################################################

FDI <- read_xlsx("FDI.xlsx")
FDI <- cbind(FDI[,c("Series Name", "Time")], FDI[,5:44])

fdi_inflow_percent <- FDI %>%
  filter(`Series Name` == 'Foreign direct investment, net inflows (% of GDP)')
fdi_outflow_percent <- FDI %>%
  filter(`Series Name` == 'Foreign direct investment, net outflows (% of GDP)')
fdi_inflow_bop <- FDI %>%
  filter(`Series Name` == 'Foreign direct investment, net inflows (BoP, current US$)')
fdi_outflow_bop <- FDI %>%
  filter(`Series Name` == 'Foreign direct investment, net outflows (BoP, current US$)')

fdi_inflow_percent <- fdi_inflow_percent %>% select(-`Series Name`)
fdi_outflow_percent <- fdi_outflow_percent %>% select(-`Series Name`)
fdi_inflow_bop <- fdi_inflow_bop %>% select(-`Series Name`)
fdi_outflow_bop <- fdi_outflow_bop %>% select(-`Series Name`)

fdi_inflow_percent <- fdi_inflow_percent %>%
  pivot_longer(-Time, names_to = "Country", values_to = "FDI_IN_%")
fdi_outflow_percent <- fdi_outflow_percent %>%
  pivot_longer(-Time, names_to = "Country", values_to = "FDI_OUT_%")
fdi_inflow_bop <- fdi_inflow_bop %>%
  pivot_longer(-Time, names_to = "Country", values_to = "FDI_IN_BoP")
fdi_outflow_bop <- fdi_outflow_bop %>%
  pivot_longer(-Time, names_to = "Country", values_to = "FDI_OUT_BoP")

merged_data <- fdi_inflow_percent %>%
  left_join(fdi_outflow_percent, by = c("Time", "Country")) %>%
  left_join(fdi_inflow_bop, by = c("Time", "Country")) %>%
  left_join(fdi_outflow_bop, by = c("Time", "Country"))

merged_data$Country <- gsub("\\[.*\\]", "", merged_data$Country)
merged_data$Country <- trimws(merged_data$Country)

names(merged_data)[1] <- "Year"

FDI <- merged_data %>%
  mutate(FDI_pct = as.numeric(`FDI_IN_%`) + as.numeric(`FDI_OUT_%`),
         FDI_bop = as.numeric(`FDI_IN_BoP`) + as.numeric(`FDI_OUT_BoP`))
colnames(FDI) <- c("year", "country_i", "FDI_IN_%", "FDI_OUT_%", "FDI_IN_BoP",
                   "FDI_OUT_BoP", "FDI_pct", "FDI_bop")
FDI <- FDI[, c("year", "country_i", "FDI_pct", "FDI_bop")]

unique(FDI$country_i)
replace_FDI <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
FDI$country_i <- str_replace_all(FDI$country_i, replace_FDI)







##############
# Final Data #
##############

final <- merge(trade_df, opns, by = c("year", "country_i"), all = TRUE)
final_d <- merge(final, CONTROL, by = c("year", "country_i"), all = TRUE)
final_dt <- merge(final_d, KOF, by = c("year", "country_i"), all = TRUE)
final_dta <- merge(final_dt, FDI, by = c("year", "country_i"), all = TRUE)
final_data <- subset(final_dta, 2022 > year & year > 1979)



# Save final_data as a CSV file
write.csv(final_data, file = "DATA.csv", row.names = FALSE)
