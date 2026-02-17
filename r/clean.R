# data ingestion and cleaning

library(readr)
library(readxl)
library(dplyr)

# Revised data
deflator <- read_csv("data/deflator.csv", 
                     col_types = cols(observation_date = col_date(format = "%Y-%m-%d")))
colnames(deflator)[2] <- "deflator"
fundsrate <- read_csv("data/fundsrate.csv", 
                      col_types = cols(observation_date = col_date(format = "%Y-%m-%d")))
gdp_pot <- read_csv("data/gdp_pot.csv",
                    col_types = cols(observation_date = col_date(format = "%Y-%m-%d")))
gdp_real <- read_csv("data/gdp_real.csv", 
                     col_types = cols(observation_date = col_date(format = "%Y-%m-%d")))

HLW_natural_r <- read_excel("data/HLW_natural_i.xlsx", 
                            col_types = c("date", "numeric"))

# set our period of interest, align observations
cutoff_start <- "1992-12-12" # I have no fucking idea why but if I set it to "1985-1-1" then filter() decides to exclude 1985-1-1 but only when filtering HLW_natural_r? 
cutoff_end <- "2025-07-01"

deflator <- deflator %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
fundsrate <- fundsrate %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
gdp_real <- gdp_real %>% filter(observation_date >= cutoff_start)
gdp_pot <- gdp_pot %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
HLW_natural_r <- HLW_natural_r %>% filter(date >= cutoff_start, date <= cutoff_end)
