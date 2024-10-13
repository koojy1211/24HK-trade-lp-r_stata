# 2024 HK Essay



# Setting Working Directory
setwd("/Users/koojy/Desktop/STUDY/학회/2024 다산금융반 35기/방중 논문/통계 분석/IV-1S")

# Load Libralies
library(dplyr) ; library(tidyr) ; library(lmtest) ; library(sandwich) ; library(purrr)



# df -> Multilateral Openness Aggregated Using R
df = read.csv("DATA.csv")

plot(df$TRADE, df$OPEN)



##############
# Full Model #
##############

m1 <- lm(TRADE ~ OPEN + GDPpc + lnPOP + AGE_dep + KOF_pol + KOF_soci + FDI_pct
         + factor(year) + factor(country_i), data = df)
clustered_se <- vcovHC(m1, type = "HC1", cluster = "country_i")

summary(m1, robust = TRUE)
### OPEN   -1.539e-07  1.881e-06  -0.082 0.934802 
coeftest(m1, vcov = clustered_se)
### OPEN   -1.5391e-07  1.4183e-06 -0.1085 0.9136044


#############
# All Cases #
#############

# Variables to Use & All Possible Combinations
variables <- c("GDPpc", "lnPOP", "AGE_dep", "KOF_pol", "KOF_soci", "FDI_pct")

combinations <- unlist(lapply(1:length(variables), function(i) {
  combn(variables, i, simplify = FALSE)
}), recursive = FALSE)

# Function to Generate the Model and Test the Coefficients
fit_model <- function(vars) {
  formula <- as.formula(paste("TRADE ~ OPEN +", paste(vars, collapse = " + "),
                              "+ factor(year) + factor(country_i)"))
  model <- lm(formula, data = df)
  
  # Robust standard errors
  coeftest_model <- coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "country_i"))
  
  # p-value & coefficient of `OPEN`
  open_coef <- coeftest_model["OPEN", "Estimate"]
  open_pvalue <- coeftest_model["OPEN", "Pr(>|t|)"]
  
  return(list(formula = formula, open_coef = open_coef, open_pvalue = open_pvalue))
}

# Model Fitting
results <- map(combinations, fit_model)

results_df <- map_df(results, ~data.frame(formula = deparse(.x$formula),
                                          open_coef = .x$open_coef,
                                          open_pvalue = .x$open_pvalue))

# Filter the Models with...
## (1) Positive `OPEN` Coefficient
## (2) Meaningful Under 5% Significance Level
best_models <- results_df %>%
  filter(open_pvalue < 0.05 & open_coef > 0) %>%
  arrange(open_pvalue)




#######################################
# Model w/ the Most Various Variables #
#######################################

bm1 <- lm(TRADE ~ OPEN + POP + AGE_dep + KOF_pol + KOF_soci
          + factor(year) + factor(country_i), data = df)
bm1.CSE <- vcovHC(bm1, type = "HC1", cluster = "country_i")
summary(bm1, robust = TRUE)
coeftest(bm1, vcov = bm1.CSE)
### OPEN   3.8305e-06  1.7256e-06  2.2199  0.026569 *

df$TRADE_IV <- predict(bm1, newdata = df)

plot(df$TRADE, df$TRADE_IV)

df <- df[, c("year", "country_i", "TRADE_IV")]
colnames(df) = c("Year", "Country", "TRADE_IV")

write.csv(df, "TRADE_IV.csv")


library(plm)
# FE
fe_model <- plm(TRADE ~ OPEN + POP + AGE_dep + KOF_pol + KOF_soci, 
               data = df, 
               index = c("country_i", "year"), 
               model = "within", 
               effect = "twoways")
summary(fe_model)

# RE
re_model <- plm(TRADE ~ OPEN + POP + AGE_dep + KOF_pol + KOF_soci, 
                data = df, 
                index = c("country_i", "year"), 
                model = "random", 
                effect = "twoways", 
                inst.method = "baltagi")

# 하우스만 검정 실행
hausman_test <- phtest(bm1_plm, re_model)

# 결과 출력
print(hausman_test)
