
library(tidyverse)
library(readxl)
library(here)
library(tidycensus)
library(janitor)
library(here)

here()

CVI <- read_rds("./Data/Processed/cvi_data_by_county_best_cols.rds") %>% 
  rename(FIPSCODE = fips_county)

colnames(CVI)

county_cat <- read.csv("./Data/Processed/counties_category.csv", 
                       colClasses = c("character","character","character","character" ))

cvi_egrid_join <- left_join(CVI,county_cat,by = "FIPSCODE") %>% 
  mutate(Category = replace_na(Category, "No plant")) %>% 
  select("state","FIPSCODE","Category","population_log","current_adult_asthma",
         "below_poverty","no_high_school_diploma",
         "riverine_flooding_annualized_frequency_log") %>% 
  filter(Category != "Other plant")

cvi_egrid_join

write.csv(cvi_egrid_join, "Data/Processed/cvi_egrid_join.csv", row.names = FALSE)
