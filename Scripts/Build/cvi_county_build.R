# Author: Sylvia Hipp
# Purpose: Aggregate CVI data at the county level, weighted by population
# Mar 25, 2026

# Setup Environment -------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(tidycensus)
library(janitor)


# Load CVI Data -----------------------------------------------------------

southern_states <- c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", 
                     "KY", "WV", "VA", "TX", "DC")

columnlist <- c("fips_code",
                "state", 
                "current_adult_asthma",
                "rail_crossings",
                "below_poverty",
                "no_high_school_diploma",
                "riverine_flooding_annualized_frequency") 

cvi_data <- read_excel(path=here('Data','Raw','cvi',"CVI_subset.xlsx"),
                       skip = 0,
                       sheet="Indicator Native Units",
                       col_names=T) %>%
  clean_names() %>% 
  filter(state %in% southern_states) %>% 
  select(columnlist)


# Get population at the census tract level --------------------------------

census_tract_pop <- get_acs(geography = "tract",
                            state = southern_states, 
                            variables = "B01003_001", 
                            geometry = FALSE) %>% 
  #mutate(county_name = str_match(NAME, ";\\s*([^;]+ County);\\s*([^;]+)")[,2]) %>% 
  select("fips_code" = GEOID, "population" = estimate)

county_pop <- get_acs(geography = "county",
                      state = southern_states, 
                      variables = "B01003_001", 
                      geometry = FALSE) %>% 
  select("fips_code" = GEOID, "county_name" = NAME, "population" = estimate)

# Join population and clean CVI data --------------------------------------

not_in_census <- cvi_data %>% anti_join(census_tract_pop)  # 5376
not_in_cvi <- census_tract_pop %>% anti_join(cvi_data)     # 11068

# ASSUMPTION: for tracts that do not have population, assign a population of 1 

cvi_data_pop <- cvi_data %>% 
  left_join(census_tract_pop, by = "fips_code") %>% 
  mutate(population = replace_na(population, 1))
  
# cvi_data_pop %>% filter(population == 1) %>% group_by(state) %>% summarize(n())


# Aggregate CVI Data to county level --------------------------------------

cvi_county <- cvi_data_pop %>% 
  mutate(fips_county = str_extract(fips_code,"^\\d{5}")) %>% 
  group_by(state, fips_county) %>% 
 # summarize tract data to weighted average, weighted by population 
  summarize(across(c("current_adult_asthma":"riverine_flooding_annualized_frequency"),
                   ~sum(population*., na.rm = TRUE)/sum(population, na.rm = TRUE))) %>% 
  ungroup() %>% 
 # add on county-level population data
  left_join(county_pop, by = c("fips_county" = "fips_code"))

