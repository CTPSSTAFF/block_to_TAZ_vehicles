
# GETTING CENSUS GEOGRAPHIES ####
region_tracts <- function(year, TAZ) {
  
  # Obtain the 2020 tracts from the tigris package.
  # The CB false (the more detailed) geometry looked like it matched up better, so we move forward with that path.
  
  tracts_MA <- tigris::tracts(state = "MA", 
                              cb = FALSE, 
                              year = year) %>% 
    st_transform(26986)
  
  tracts_RI <- tigris::tracts(state = "RI", 
                              cb = FALSE, 
                              year = year) %>% 
    st_transform(26986)
  
  tracts_NH <- tigris::tracts(state = "NH", 
                              cb = FALSE, 
                              year = year) %>% 
    st_transform(26986)
  
  tracts_together <- bind_rows(tracts_MA, tracts_RI, tracts_NH) 
  
  # Clip the tracts to match the geometry of the TAZ file. This will eliminate coastal waters but retain inland waters. 
  #This is a useful starting place to have a similar coast line. This will also make our `erase_water` function work on a more similar set of water.
  # Summarize() creates one shape for all of the TAZs.  When intersecting with the tracts, only the areas that intersect with the combined TAZ area remains. 
  
  tracts_nocoast <- 
    st_intersection(tracts_together, TAZ |> summarize()) |> 
    st_collection_extract("POLYGON")
  
  
  # Review the differences. It looks like what we'd expect. Coastal waters are not in the dataset anymore. 
  # Overwrite the original file with the no_coast version and clean up the workspace.
  
  tracts <- tracts_nocoast %>% 
    group_by(GEOID, ALAND, AWATER) |> 
    summarize()
  
  
  return(tracts)
}

region_bg <- function(year, TAZ) {
  
  BG_MA <- tigris::block_groups(state = "MA", 
                          cb = FALSE, 
                          year = year) %>% 
    st_transform(26986)
  
  BG_RI <- tigris::block_groups(state = "RI", 
                          cb = FALSE, 
                          year = year) %>% 
    st_transform(26986)
  
  BG_NH <- tigris::block_groups(state = "NH", 
                          cb = FALSE, 
                          year = year) %>% 
    st_transform(26986)
  
  block_groups_together <- bind_rows(BG_MA, BG_RI, BG_NH)
  
  
  # Clip the tracts to match the geometry of the TAZ file. This will eliminate coastal waters but retain inland waters. 
  #This is a useful starting place to have a similar coast line. This will also make our `erase_water` function work on a more similar set of water.
  # Summarize() creates one shape for all of the TAZs.  When intersecting with the tracts, only the areas that intersect with the combined TAZ area remains. 
  
  BG_nocoast <- 
    st_intersection(block_groups_together, TAZ |> summarize()) |> 
    st_collection_extract("POLYGON")
  
  
  # Review the differences. It looks like what we'd expect. Coastal waters are not in the dataset anymore. 
  # Overwrite the original file with the no_coast version and clean up the workspace.
  
  block_groups <- BG_nocoast %>% 
    group_by(GEOID, ALAND, AWATER) |> 
    summarize()
  
  
  return(block_groups)
}



# INTERSECTIONS ####

intersection_func <- function(TAZ, geog) {
  
  intersections <- st_intersection(TAZ, geog) %>%
    st_collection_extract("POLYGON") %>% 
    rowid_to_column("Intersection_ID") # unique ID for each intersection geometry
  
  intersections$INTERSECTION_TOTAL_AREA <- st_area(intersections)
  
  
  return(intersections)
}


# GET MINORITY DATA ####
# we used the ACS data here instead of just the census data because we wanted margins
# of error, and the decennial doesn't provide them. 
min_data <- function(geog) {
  
  
  #B03002_003 | Estimate!!Total:!!Not Hispanic or Latino:!!White alone
  #B03002_001 | Estimate!!Total: HISPANIC OR LATINO ORIGIN BY RACE
  
  min <- get_acs(geography = geog,
                      variable = "B03002_003", 
                      summary_var = "B03002_001", 
                      state= c("MA", "RI", "NH"), # prep for adding states
                      year= 2021)  %>% 
    rename(nonmin_pop = estimate, tot_pop = summary_est, 
           nonmin_moe = moe, tot_moe = summary_moe ) %>% 
    select(-variable) %>% 
    mutate(GEOID = as.numeric(GEOID))
  
  return(min)
}

# GET INCOME DATA ####
inc_data <- function(geog) {
  
  # C17002_001 Estimate!!Total: RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
  # C17002_008 Estimate!!Total:!!2.00 and over RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
  inc_acs <- paste0("C17002_", str_pad(c(1,8), width = 3, side = "left", pad = 0))
  names(inc_acs)<- c("total", "nonlowinc")
  
  
  acs_inc_raw <- get_acs(geography = geog,
                         variables = inc_acs,
                         state = c("MA", "RI", "NH"),
                         geometry = F,
                         year = 2021)
  
  inc_acs <- acs_inc_raw %>% 
    group_by(GEOID, variable) %>% 
    summarise(est= sum(estimate),
              moe = moe_sum(moe, est)) %>%
    pivot_wider(id_cols = GEOID, names_from= variable, values_from = c(est, moe)) 
  
  
  return(inc_acs)
}

# GET TOTAL POPULATION DATA ####
pop_data <- function(geog) {
  
  dec_var <- "P2_001N"
  
  dec_raw <- get_decennial(geography = geog,
                           variables = dec_var,
                           state = c("MA", "RI", "NH"),
                           geometry = FALSE,
                           year = 2020) %>% 
    select(GEOID, NAME, pop_dec = value)
  
  dec_raw$GEOID <- as.numeric(dec_raw$GEOID)
  
  return(dec_raw)
}



# TAZ RESULTS SUMMARY ####

results_min <- function(TAZ_results) {
  
  TAZ_results_summ <- TAZ_results %>% 
    group_by(ID) %>% 
    # allocation method
    summarize(n = n(), TAZ_nonmin = sum(nonmin_alloc, na.rm=TRUE),
              TAZ_tot =    sum(tot_alloc, na.rm=TRUE),
              
              TAZ_nonmin_moe = moe_sum(nonmin_moe_alloc, estimate = nonmin_alloc),
              TAZ_tot_moe =    moe_sum(tot_moe_alloc, estimate = tot_alloc)) %>% 
    mutate(nonmin_pct = TAZ_nonmin / TAZ_tot, 
           min_pct = 1 - nonmin_pct) %>%
    select(-nonmin_pct) %>% 
    # This relies on the MOE of a P being equal to the moe of (1-P).
    mutate(min_pct_moe = moe_prop(num = TAZ_nonmin, denom = TAZ_tot, 
                                  moe_num = TAZ_nonmin_moe, moe_denom = TAZ_tot_moe))  %>% 
    mutate(combined_moe_pct = paste0(scales::percent(min_pct, 0.1), 
                                     " ± ", 
                                     scales::percent(min_pct_moe, 0.1)))
  
  return(TAZ_results_summ)
  
}

results_inc <- function(TAZ_results) {
  
  TAZ_results_summ <- TAZ_results %>% 
    group_by(ID) %>% 
    # allocation method
    summarize(n = n(), TAZ_non_lowinc = sum(non_lowinc_alloc, na.rm=TRUE),
              TAZ_tot = sum(tot_alloc, na.rm=TRUE),
              
              TAZ_non_lowinc_moe = moe_sum(non_lowinc_moe_alloc, estimate = non_lowinc_alloc),
              TAZ_tot_moe =    moe_sum(tot_moe_alloc, estimate = tot_alloc)) %>% 
    mutate(non_lowinc_pct = TAZ_non_lowinc / TAZ_tot, 
           lowinc_pct = 1 - non_lowinc_pct) %>%
    select(-non_lowinc_pct) %>% 
    # This relies on the MOE of a P being equal to the moe of (1-P).
    mutate(lowinc_pct_moe = moe_prop(num = TAZ_non_lowinc, denom = TAZ_tot, 
                                  moe_num = TAZ_non_lowinc_moe, moe_denom = TAZ_tot_moe))  %>% 
    mutate(combined_moe_pct = paste0(scales::percent(lowinc_pct, 0.1), 
                                     " ± ", 
                                     scales::percent(lowinc_pct_moe, 0.1)))
  
  return(TAZ_results_summ)
  
}

results_pop <- function(TAZ_results) {
  
  TAZ_results_summ <- TAZ_results %>% 
    group_by(ID) %>% 
    # allocation method
    summarize(n = n(), TAZ_pop_dec = sum(pop_dec_alloc, na.rm=TRUE))
  
  return(TAZ_results_summ)
}
