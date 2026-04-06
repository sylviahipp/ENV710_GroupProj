
library(tidyverse)
library(readxl)
library(here)
library(tidycensus)
library(janitor)
library(here)

here()

CVI <- read_rds("./Data/Processed/cvi_data_by_county.rds") %>% 
  rename(FIPSCODE = fips_county)

county_cat <- read.csv("./Data/Processed/counties_category.csv", 
                       colClasses = c("character","character","character","character" ))

cvi_egrid_join <- left_join(CVI,county_cat,by = "FIPSCODE") %>% 
  mutate(Category = replace_na(Category, "No plant")) %>% 
  select("state","FIPSCODE","Category","population","current_adult_asthma",
         "rail_crossings","below_poverty","no_high_school_diploma",
         "riverine_flooding_annualized_frequency")

write.csv(cvi_egrid_join, "Data/Processed/cvi_egrid_join.csv", row.names = FALSE)
