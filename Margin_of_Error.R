library(tidyverse)
library(tidycensus)
library(tigris)
library(mapview)
library(sf)
library(units)
library(rgeos)
library(sp)
library(mapview)
library(leafsync)


# Obtaining VRE Data
# https://www2.census.gov/programs-surveys/acs/replicate_estimates/2021/data/5-year/140/
# B03002_25.csv.zip

acs21 <- load_variables(2021, "acs5", cache = T)

VRE_table <- read.csv("data/base/B03002_25.csv")


# Grab the rows that we need

vars <- c(4:9, 12)

VRE_table_min <- VRE_table %>% 
  filter(ORDER %in% vars) %>% 
  filter(GEOID == "1400000US25001010100")

# Find MOE by using replicate estimates
VREs <- VRE_table_min %>% 
  select(starts_with("Var_Rep"))

VRE_sums <- tibble(sums = colSums(VREs) ) %>% 
  mutate(diff = sums - sum(VRE_table_min$ESTIMATE)) %>% 
  mutate(diff_squared = diff ** 2)

sum_squares <- sum(VRE_sums$diff_squared)
variance <- (4 / 80) * sum_squares  


MOE <- 1.645 * sqrt(variance)  

MOE_check <- VRE_table_min$MOE ** 2 %>% 
  sum() %>% 
  sqrt()
# MOE and MOE_check don't match exactly

