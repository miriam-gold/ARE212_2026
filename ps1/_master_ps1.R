# ============================================================
# ARE 212, Spring 2026
# PS 1
# Author: David Cai, Miriam Gold, Ella Moxley, Geoffrey Yip
# Date: Feb 25, 2026
# ============================================================
#ttktk
# Set up ===================

library(WDI)
library(tidyverse)
library(magrittr)
library(fs)


# Paths ==================
path <- "/Users/miriamgold/projects/ARE212_2026/ps1"
path_functions <- file.path(path, "functions")
path_data <- file.path(path, "data")

# Source custom functions 
dir_walk(path_functions, source)

# Data setup ==============

iso_codes <- 
  read_csv("https://gist.githubusercontent.com/radcliff/f09c0f88344a7fcef373/raw/2753c482ad091c54b1822288ad2e4811c021d8ec/wikipedia-iso-country-codes.csv")

wb_vars <- c("EN.GHG.CO2.MT.CE.AR5", "NY.GDP.MKTP.KD", "SP.POP.TOTL", "SH.XPD.GHED.GD.ZS")

countries <- 
  iso_codes |>
  pull("Alpha-2 code")

# Q1. Read in data ========================
wdi_data_raw <- 
  WDI(
    country = countries,
    indicator = wb_vars, 
    start = 2010, 
    end = 2010
  )

# Q2. Rename variables ====================
wdi_data_clean <- 
  wdi_data_raw |>
  rename(
    CO2 = "EN.GHG.CO2.MT.CE.AR5",
    GDP = "NY.GDP.MKTP.KD",
    POP = "SP.POP.TOTL",
    GOV = "SH.XPD.GHED.GD.ZS"
  ) |>
  drop_na()

# Q3. Summary stats
wdi_summary_stats <- 
  wdi_data_clean |>
   pivot_longer(cols = c("CO2", "GDP", "POP"), names_to = "var") |>
   group_by(var) |>
   summarise(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
   )

# Q4. Histogram of CO2 and GDP
wdi_data_clean %>%
   pivot_longer(cols = c("CO2", "GDP", "POP"), names_to = "var") %>%
  filter(var %in% c("CO2", "GDP")) %>%
  ggplot(aes(x = value, fill = var)) +
  geom_histogram(bins = 15, position = "dodge") +
  scale_fill_discrete(name = NULL) +
  theme_bw()

# Q5. Plot CO2 against GDP
wdi_data_clean %>%
  ggplot(aes(x = GDP, y = CO2)) +
  geom_point() +
  scale_x_continuous("Gross Domestic Product (constant USD)") +
  scale_y_continuous("CO2 Emissions (mt)") +
  labs(
    title = "Country-level Carbon Emissions against GDP (2010)"
  ) +
  theme_bw()

# Q6/7. Per capita variables ===================
wdi_per_capita <-
  wdi_data_clean %>%
  mutate(
    CO2pc = CO2/POP,
    GDPpc = GDP/POP
  )

# Q8. Plot CO2/pop against GDP/pop ===============
wdi_per_capita %>%
  ggplot(aes(x = GDPpc, y = CO2pc)) +
  geom_point() +
  scale_x_continuous("GDP (constant USD/capita)") +
  scale_y_continuous("CO2 Emissions (mt/capita)") +
  labs(
    title = "Per capita carbon emissions and GDP (2010)"
  ) +
  theme_bw()

# Q9. Demeaned variables =========================
wdi_per_capita_demeaned <- 
  wdi_per_capita %>%
  mutate(
    CO2pcdev = CO2pc-mean(CO2pc),
    GDPpcdev = GDPpc-mean(GDPpc)
  )

# Q10. Scatterplot of demeaned variables ============
wdi_per_capita_demeaned %>%
  ggplot(aes(x = GDPpcdev, y = CO2pcdev)) +
  geom_point() +
  scale_x_continuous("Demeaned GDP (constant USD/capita)") +
  scale_y_continuous("Demeaned CO2 Emissions (mt/capita)") +
  labs(
    title = "Per capita demeaned carbon emissions and GDP (2010)"
  ) +
  theme_bw()

# Q11. Natural log variables ===================
wdi_per_capita_log <-
  wdi_per_capita_demeaned %>%
  mutate(
    CO2pcln = log(CO2pc),
    GDPpcln = log(GDPpc)
  )

# Q12. Scatterplot of logged variables ============
wdi_per_capita_log %>%
  ggplot(aes(x = GDPpcln, y = CO2pcln)) +
  geom_point() +
  scale_x_continuous("Demeaned GDP (constant USD/capita)") +
  scale_y_continuous("Demeaned CO2 Emissions (mt/capita)") +
  labs(
    title = "Per capita demeaned carbon emissions and GDP (2010)"
  ) +
  theme_bw()

# Q13. Export data ==================
  wdi_per_capita_log %>%
    write_csv(file = file.path(path_data, "are212_gold_ps1_wdi_clean.csv"))

# Q14. Regress CO2pc on GDPpc ==================

ols(
  wdi_per_capita_log$GDPpc, 
  wdi_per_capita_log$CO2pc
)$beta # coefficient = 2.37e-10

ols(
  wdi_per_capita_log$GDPpc, 
  wdi_per_capita_log$CO2pc*1000
)$beta # coefficient = 2.37e-7 (the coefficient has been multiplied by 1000)

ols_co2_gdp <- 
  ols(
    wdi_per_capita_log$GDPpc/1000, 
    wdi_per_capita_log$CO2pc*1000
  )
ols_co2_gdp$beta # coefficient = 2.37e-4 (the coefficient has been been multiplied by a further factor of 1000)

# Q15. Model stats and predicted values ==============
ols_co2_gdp %>% names()

## N = 197
nrow(wdi_per_capita_log)

## Degrees of freedom: N-k
df <- n-1 # k=1 because there is no intercept

## beta = 0.000237
b <- ols_co2_gdp$beta

## R^2 uncentered = 0.26
ols_co2_gdp$r2_uc

## R^2 = 0.25
ols_co2_gdp$r2

## Adjusted R^2 = 0.25
ols_co2_gdp$r2_bar

## s^2 9.46e-05
ols_co2_gdp$s2

predicted_vs_actual <-
  data.frame(
    y_hat = ols_co2_gdp$y_hat,
    y = ols_co2_gdp$Y
)

predicted_vs_actual %>%
  ggplot(aes(x = y, y = y_hat)) +
  geom_point() +
  geom_abline() +
  coord_fixed() +
  scale_y_continuous(limits = c(0, 0.03)) +
  scale_x_continuous(limits = c(0, 0.12)) +
  labs(
    title = "Without intercept"
  )

# Q16. Regression with intercept =====================
ols_co2_gdp_intercept <- 
  ols(
    wdi_per_capita_log$GDPpc/1000, 
    wdi_per_capita_log$CO2pc*1000,
    intercept = TRUE
  )

## beta
b_i <- ols_co2_gdp_intercept$beta

## R^2 uncentered = 0.3
ols_co2_gdp_intercept$r2_uc

## R^2 = 0.12
ols_co2_gdp_intercept$r2

## Adjusted R^2 = 0.11
ols_co2_gdp_intercept$r2_bar

## s^2 = 9.02-05
ols_co2_gdp_intercept$s2

predicted_vs_actual_intercept <-
  data.frame(
    y_hat = ols_co2_gdp_intercept$y_hat,
    y = ols_co2_gdp_intercept$Y,
    x = ols_co2_gdp_intercept$X[,2]
)

predicted_vs_actual_intercept %>%
  ggplot(aes(x = y, y = y_hat)) +
  geom_point() +
  geom_abline() +
  coord_fixed() +
  scale_y_continuous(limits = c(0, 0.03)) +
  scale_x_continuous(limits = c(0, 0.12)) +
  labs(
    title = "With intercept"
  )

# Q17. Regression, quadratic ==========================
wdi_per_capita_quad <-
  wdi_per_capita_log %>%
  mutate(
    CO2pc_thou = CO2pc*1000,
    GDPpc_thou = GDPpc/1000,
    GDPpc_thou_2 = (GDPpc_thou^2)
  )

ols_co2_gdp_quad <- 
  ols(
    x = wdi_per_capita_quad %>% select(GDPpc_thou, GDPpc_thou_2),
    y = wdi_per_capita_quad %>% select(CO2pc_thou),
    intercept = TRUE
  )

predicted_vs_actual_quad <-
  data.frame(
    y_hat = ols_co2_gdp_quad$y_hat,
    y = ols_co2_gdp_quad$Y,
    x = ols_co2_gdp_quad$X[,2]
) %>%
  rename(y_hat = 1, y = 2, x = 3)

predicted_vs_actual_quad %>%
  ggplot(aes(x = y, y = y_hat)) +
  geom_point() +
  geom_abline() +
  coord_fixed() +
  scale_y_continuous(limits = c(0, 0.03)) +
  scale_x_continuous(limits = c(0, 0.12)) +
  labs(
    title = "With intercept + quadratic term"
  )

# Q18. Demeaned ===================================
wdi_per_capita_demean <-
  wdi_per_capita_quad %>%
  mutate(
    CO2pc_thou_demean = CO2pc_thou - mean(CO2pc_thou),
    GDPpc_thou_demean = GDPpc_thou - mean(GDPpc_thou),
    GDPpc_thou_2_demean = GDPpc_thou_2 - mean(GDPpc_thou_2)
  )

ols_co2_gdp_demean <-
  ols(
    x = wdi_per_capita_demean %>% select(GDPpc_thou_demean, GDPpc_thou_2_demean),
    y = wdi_per_capita_demean %>% select(CO2pc_thou_demean),
    intercept = FALSE
  )
  
# They're the same, whoa!
ols_co2_gdp_quad$beta
ols_co2_gdp_demean$beta


# Q19. More power of FWL ===============================
ols_co2_gdp_q19 <- 
  ols(
    x = wdi_per_capita_quad %>% select(GDPpc_thou),
    y = wdi_per_capita_quad %>% select(CO2pc_thou),
    intercept = TRUE
  )

resid_q19 <- ols_co2_gdp_q19$e

ols_1_gdp <- 
  ols(
    x = wdi_per_capita_quad %>% select(GDPpc_thou),
    y = matrix(1, nrow = nrow(wdi_per_capita_quad)),
    intercept = FALSE
  )

ols_1_gdp_resid <- ols_1_gdp$e


ols_gdp2_gdp <- 
  ols(
    x = wdi_per_capita_quad %>% select(GDPpc_thou),
    y = wdi_per_capita_quad %>% select(GDPpc_thou_2),
    intercept = FALSE
  )

ols_gdp2_gdp_resid <- ols_gdp2_gdp$e

resid_matrix <- cbind(ols_1_gdp_resid, ols_gdp2_gdp_resid)

ols_resid_on_resid <-
  ols(
    x = resid_matrix,
    y = resid_q19,
    intercept = FALSE
  )

ols_resid_on_resid$beta
