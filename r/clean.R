# data ingestion and cleaning

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

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
# rstar has the earliest observation at 2005-01-01 so
cutoff_start <- "2004-12-31" # I have no fucking idea why but if I set it to the exact date then filter() decides to exclude it but only when filtering HLW_natural_r? 
cutoff_end <- "2025-04-01"

deflator <- deflator %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
fundsrate <- fundsrate %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
gdp_real <- gdp_real %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
gdp_pot <- gdp_pot %>% filter(observation_date >= cutoff_start, observation_date <= cutoff_end)
HLW_natural_r <- HLW_natural_r %>% filter(date >= cutoff_start, date <= cutoff_end)

# Same thing for real-time data
rt_ngdp <- read_excel("data/rt_ngdp.xlsx")
# convert quarters to dates (01-01 as Q1)
rt_ngdp$date <- gsub(":Q1$", "-01-01", rt_ngdp$date)
rt_ngdp$date <- gsub(":Q2$", "-04-01", rt_ngdp$date)
rt_ngdp$date <- gsub(":Q3$", "-07-01", rt_ngdp$date)
rt_ngdp$date <- gsub(":Q4$", "-10-01", rt_ngdp$date)

rt_rgdp <- read_excel("data/rt_rgdp.xlsx")
rt_rgdp$date <- gsub(":Q1$", "-01-01", rt_rgdp$date)
rt_rgdp$date <- gsub(":Q2$", "-04-01", rt_rgdp$date)
rt_rgdp$date <- gsub(":Q3$", "-07-01", rt_rgdp$date)
rt_rgdp$date <- gsub(":Q4$", "-10-01", rt_rgdp$date)

rt_ngdp <- rt_ngdp %>% filter(date >= cutoff_start, date <= cutoff_end)
rt_rgdp <- rt_rgdp %>% filter(date >= cutoff_start, date <= cutoff_end)

# NA value for 1995-10-01, this is the only missing value in the set. just duplicate the previous one, not a huge effect here
rt_ngdp <- rt_ngdp %>% fill(d)
rt_rgdp <- rt_rgdp %>% fill(d)


# there are missing values in rt_rstar during COVID
rt_rstar <- read_csv("data/rt_rs_clean.csv") # cleaned version, starts at 2005
# rt_rstar <- read_csv("data/rt_rs_clean_interp.csv") # version with missing values filled in and pre-2005 data according to 2005 vintage
rt_rstar <- filter(rt_rstar, date >= cutoff_start)
