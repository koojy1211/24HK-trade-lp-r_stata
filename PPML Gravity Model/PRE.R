# 2024 HK ESSAY



# Setting Working Directory
setwd("/Users/koojy/Desktop/STUDY/학회/2024 다산금융반 35기/방중 논문/통계 분석/IV")


# Load Libraries
library(readxl) ; library(ggplot2) ; library(tseries) ; library(xts)
library(date) ; library(lubridate) ; library(tsDyn) ; library(zoo)
library(dplyr) ; library(tidyr) ; library(stringr)

# Load Data
AREA = read_excel("AREA.xlsx")
BOR = read_excel("BOR.xlsx")
D = read_excel("D.xlsx")
DIST = read_excel("DIST.xlsx")
FINDIST = read_excel("FINDIST.xlsx")
POP = read_excel("POP.xlsx")

# Delete [~] from the Country Names
clean_country_names <- function(df, country_col) {
  df %>%
    mutate(!!sym(country_col) := sub(" \\[.*\\]", "", !!sym(country_col)))
}



########
# AREA #
########

AREA_long <- AREA %>%
  pivot_longer(-Time, names_to = "country", values_to = "area") %>%
  rename(year = Time)
colnames(AREA_long) = c("year", "country_i", "AREA")
AREA_long <- clean_country_names(AREA_long, "country_i")

unique(AREA_long$country_i)
replace_area <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
AREA_long$country_i <- str_replace_all(AREA_long$country_i, replace_area)



#######
# BOR #
#######

BOR_long <- BOR %>%
  pivot_longer(-...1, names_to = "country", values_to = "border") %>%
  rename(year = ...1)
colnames(BOR_long) = c("country_i", "country_j", "BOR")

unique(BOR_long$country_i)
unique(BOR_long$country_j)
replace_bor <- c(
  "Turkey" = "Turkiye",
  "Venezuela" = "Venezuela, RB"
)
BOR_long$country_i <- str_replace_all(BOR_long$country_i, replace_bor)
BOR_long$country_j <- str_replace_all(BOR_long$country_j, replace_bor)



#####
# D #
#####

D_long <- D %>%
  pivot_longer(-Time, names_to = "country", values_to = "disaster") %>%
  rename(year = Time)
colnames(D_long) = c("year", "country_i", "D")
D_long <- clean_country_names(D_long, "country_i")

unique(D_long$country_i)
replace_d <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
D_long$country_i <- str_replace_all(D_long$country_i, replace_d)



########
# DIST #
########

DIST_long <- DIST %>%
  pivot_longer(-...1, names_to = "country", values_to = "distance") %>%
  rename(year = ...1)
colnames(DIST_long) = c("country_i", "country_j", "DIST")

unique(DIST_long$country_i)
unique(DIST_long$country_j)
replace_dist <- c(
  "Turkey" = "Turkiye",
  "Venezuela" = "Venezuela, RB"
)
DIST_long$country_i <- str_replace_all(DIST_long$country_i, replace_dist)
DIST_long$country_j <- str_replace_all(DIST_long$country_j, replace_dist)



###########
# FINDIST #
###########

colnames(FINDIST) = c("country_i", "FINDIST")

unique(FINDIST$country_i)
replace_findist <- c(
  "Turkey" = "Turkiye",
  "Venezuela" = "Venezuela, RB"
)
FINDIST$country_i <- str_replace_all(FINDIST$country_i, replace_findist)



#######
# POP #
#######

POP_long <- POP %>%
  pivot_longer(-Time, names_to = "country", values_to = "population") %>%
  rename(year = Time)
colnames(POP_long) = c("year", "country_i", "POP")
POP_long <- clean_country_names(POP_long, "country_i")

unique(POP_long$country_i)
replace_pop <- c(
  "Hong Kong SAR, China" = "Hong Kong",
  "Korea, Rep." = "South Korea"
)
POP_long$country_i <- str_replace_all(POP_long$country_i, replace_pop)



##############
# Final Data #
##############

# Data Merge
data_r <- DIST_long %>%
  inner_join(BOR_long, by = c("country_i", "country_j"))

# Merge country-specific data
data_e <- POP_long %>%
  inner_join(D_long, by = c("country_i", "year")) %>%
  inner_join(AREA_long, by = c("country_i", "year"))

# No year, No Country Relations - FINDIST
data_a <- FINDIST





# Rename columns to match the desired format
data_a <- data_a %>% rename(country_i = country_i, FINDIST = FINDIST)
data_e <- data_e %>% rename(country_i = country_i, year = year, POP = POP, D_j_t = D, AREA_j = AREA)
data_r <- data_r %>% rename(country_i = country_i, country_j = country_j, DIST_ij = DIST, BOR_ij = BOR)

str(data_a)
str(data_e)
str(data_r)

# Convert AREA_j to numeric
data_e$AREA_j <- as.numeric(gsub("[^0-9]", "", data_e$AREA_j))

# Expand data_r to include all years from data_e
years <- unique(data_e$year)
data_r_expanded <- data_r %>%
  expand(country_i, country_j, year = years) %>%
  left_join(data_r, by = c("country_i", "country_j"))

# Merge DATA_E with itself to get POP_j and AREA_j
data_e_j <- data_e %>% rename(country_j = country_i, POP_j = POP, AREA_j = AREA_j, D_j_t = D_j_t)
data_merged <- merge(data_r_expanded, data_e, by = c("country_i", "year"))
data_merged <- merge(data_merged, data_e_j, by = c("country_j", "year"))

# Select and reorder columns to match the desired format
final_data <- data_merged %>%
  select(year, country_i, country_j, POP_i = POP, POP_j, DIST_ij, BOR_ij,
         D_j_t = D_j_t.x, AREA_j = AREA_j.x)
colnames(final_data) = c("year", "country_j", "country_i", "POP_j", "POP_i",
                         "DIST_ij", "BOR_ij", "D_j_t", "AREA_j")
final_data = final_data %>%
  select(year, country_i, country_j, POP_i, POP_j, DIST_ij, BOR_ij, D_j_t, AREA_j)

final_data = final_data %>%
  arrange(year, country_i, country_j)

unique(final_data$country_i) %in% unique(final_data$country_j)





###################################
# Final Data Including TRADE Data #
###################################

# Load Data Files
TRADE <- read.csv("DATA_Trade.csv")

# Arrange
final_data <- final_data %>%
  arrange(year, country_i, country_j) %>%
  filter(country_i != country_j)

TRADE <- TRADE %>%
  arrange(year, country_i, country_j)

unique(final_data$country_i) %in% unique(TRADE$country_i)
unique(final_data$country_j) %in% unique(TRADE$country_j)


# Only Leave Common Countries
common_countries <- intersect(unique(final_data$country_i), unique(TRADE$country_i))

final_data <- final_data %>%
  filter(country_i %in% common_countries & country_j %in% common_countries)
TRADE <- TRADE %>%
  filter(country_i %in% common_countries & country_j %in% common_countries)



# Merge Data Frame
final_data <- merge(final_data, TRADE, by = c("year", "country_i", "country_j"), all = TRUE)
dt <- final_data %>%
  select(-X, -EXPORTS, -IMPORTS)



# Merge FINDIST
dt <- dt %>%
  left_join(data_a, by = c("country_j" = "country_i")) %>%
  rename(FINDIST_j = FINDIST)
colnames(dt) = c("year", "country_i", "country_j", "POP_i", "POP_j", "DIST", "BOR", "D",
                 "AREA_j", "FLOW", "FINDIST_j")

write.csv(dt, file = "Final_DATA.csv")
