# Author: Sylvia Hipp
# Purpose: Clean eGRID dataset for analysis with CVI data
# Feb 25, 2026

# Setup Environment -------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)

# Read raw data -----------------------------------------------------------

plnt23_raw <- read_excel(here("Data/Raw/egrid/egrid2023_data_rev2.xlsx"), 
                         sheet = "PLNT23", skip=1)

glimpse(plnt23_raw)

# Process data ------------------------------------------------------------

# Filter plants based on: 
# Southeastern states (plus Texas)
# Plants with Capacity Factor > 1% and Nameplate Capacity > 1 MW
# Exclude plants where county info is missing

southern_states <- c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", 
                     "KY", "WV", "VA", "TX", "DC")
cf_limit <- 0.01
npcap_limit <- 1

plnt23 <- plnt23_raw %>% 
  filter(PSTATABB %in% southern_states, 
         NAMEPCAP >= npcap_limit, 
         CAPFAC >= cf_limit, 
         !is.na(FIPSCNTY))

counties_count <- plnt23 %>% 
  group_by(PSTATABB, FIPSCNTY, CNTYNAME, PLFUELCT) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("PSTATABB", "FIPSCNTY", "CNTYNAME"), 
              names_from = "PLFUELCT", 
              values_from = "n") 
  
counties_mw <- plnt23 %>% 
  group_by(PSTATABB, FIPSCNTY, CNTYNAME, PLFUELCT) %>% 
  summarize(capacity = sum(NAMEPCAP)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("PSTATABB", "FIPSCNTY", "CNTYNAME"), 
              names_from = "PLFUELCT", 
              values_from = "capacity") 

counties_count$allFF <- rowSums(counties_count[c("GAS", "COAL", "OIL", "OFSL")],
                                na.rm = TRUE)
counties_count$windsolar <- rowSums(counties_count[c("WIND", "SOLAR")],
                                    na.rm = TRUE)

counties_cat <- counties_count %>% 
  mutate(Category = case_when(
    allFF > 0 & windsolar > 0 ~ "Contains FF and solar/wind ", 
    allFF > 0 ~ "Contains FF",
    windsolar > 0 ~ "Contains solar/wind",
    allFF == 0 & windsolar ==0 ~"Neither"
  )) %>% 
  select(PSTATABB,FIPSCNTY,CNTYNAME,Category)

write.csv(counties_cat, "Data/Processed/counties_category.csv", row.names = FALSE)


