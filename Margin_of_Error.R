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

library(tidycensus)

# Obtaining VRE Data
# https://www2.census.gov/programs-surveys/acs/replicate_estimates/2021/data/5-year/140/
# B03002_25.csv.zip

acs21 <- load_variables(2021, "acs5", cache = T)

VRE_table <- read.csv("data/base/B03002_25.csv")


# Grab the rows that we need

vars <- c(4:9, 12)


# Find MOE of all minority for one tract by using replicate estimates

VRE_table_min <- VRE_table %>% 
  filter(ORDER %in% vars) %>% 
  filter(GEOID == "1400000US25001010100")

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
# MOE and MOE_check don't match exactly (257 to 263)

# When summing margins of error with multiple 0 estimates, only keep one.
# This process may fail for anything but a simple example. 
# Located at the bottom of: https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2015.pdf
# ISSUES WITH APPROXIMATING THE STANDARD ERROR OF LINEAR 
# COMBINATIONS OF MULTIPLE ESTIMATES

MOE_check_keep_one_zero <- VRE_table_min |> 
  distinct(ESTIMATE, MOE) |> 
  mutate(MOE_SQ = MOE ** 2) |> 
  summarize(SUM_SQ = sum(MOE_SQ)) |> 
  pull() |> 
  sqrt()

vars_acs <- tidycensus::load_variables(year = 2021, dataset = "acs5")
tc_tabs <- paste0("B03002_", str_pad( c(4:9, 12), width = 3,side = "left", pad = 0))
tc_method <- tidycensus::get_acs(geography = "tract", 
                                 state = "MA", 
                                 variables = tc_tabs,
                                 year = 2021) |> 
  inner_join(vars_acs, by = c("variable" = "name")) |> 
  filter(GEOID == "25001010100")

tc_sum <- tc_method |> 
  summarize(MOE = moe_sum(moe = moe, estimate = estimate)) |> 
  pull()

# Check if these match. They do!
MOE_check_keep_one_zero == tc_sum

# All tracts ---------------
# Find MOE of non-hispanic/latino white for each tract by using replicate estimates

VRE_table_white <- VRE_table %>% 
  filter(ORDER == 3)

VREs_white <- VRE_table_white %>% 
  select(starts_with("Var_Rep"))

VRE_sums_white <- tibble(sums = colSums(VREs_white) ) %>% 
  mutate(diff = sums - sum(VRE_table_white$ESTIMATE)) %>% 
  mutate(diff_squared = diff ** 2)

sum_squares_white <- sum(VRE_sums_white$diff_squared)
variance_white <- (4 / 80) * sum_squares_white

MOE_white <-  1.645 * sqrt(variance_white)

MOE_white_check <- VRE_table_white$MOE ** 2 %>% 
  sum() %>% 
  sqrt()

MOE_white_check_keep_one_zero <- VRE_table_white |> 
  distinct(ESTIMATE, MOE) |> 
  mutate(MOE_SQ = MOE ** 2) |> 
  summarize(SUM_SQ = sum(MOE_SQ)) |> 
  pull() |> 
  sqrt()


# Margin of Error Example
# 
demo <- 
  tibble(muni = c("Somerville", "Cambridge"),
         under5 = c(2905, 4000),
         moe = c(797, 800),
         share = c(0.3, 0.4))

demo <- demo |> 
  mutate(minval = under5 - moe,
         maxval = under5 + moe) |> 
  mutate(under5_alloc = under5 * share,
         minval_alloc = minval * share,
         maxval_alloc = maxval * share) |> 
  mutate(tc_moe_alloc = moe * share)

# tc_moe_sum uses the allocated share of the MOE then performs the 
# sum of squares. 
# 
# moe_alloc uses the share of the min/max values (calcd by adding the MOEs)
# then finds the MOE after adding the shares. 
# in this case, and probably all cases, moe_alloc should be greater than or 
# equal to tc_moe_sum.
# 
demo_summ <- demo |> 
  summarize(
    tot_under5 = sum(under5), 
    moe = moe_sum(moe, estimate = under5),
    
    tc_moe_sum = moe_sum(moe = tc_moe_alloc, estimate = under5_alloc),
    
    under5_sum = sum(under5_alloc),
    minval_sum = sum(minval_alloc),
    maxval_sum = sum(maxval_alloc)) |> 
  mutate(moe_alloc = under5_sum - minval_sum)
