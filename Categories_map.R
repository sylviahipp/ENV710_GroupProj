# Author: Maeve Gualtieri-Reed
# Purpose: Create a map of Southeastern US counties by plant category
# March 14, 2026


# Setup Environment -------------------------------------------------------
library(tidyverse)
library(readxl)
library(here)
#install.packages("sf")
library(sf)

#Read in Data -------------------------------------------------------------
southern_fps <- c(
  "12","13","01","28","22","05","47","37","45","21","54","51","48")

SE_Counties <- st_read(here('Data/Raw/counties/cb_2018_us_county_20m.shp')) %>% 
  filter(STATEFP %in% southern_fps) %>% 
  rename(FIPSCNTY = COUNTYFP) %>% 
  mutate(PSTATABB = case_when(
    STATEFP == "12" ~ "FL",
    STATEFP == "13" ~ "GA",
    STATEFP == "01" ~ "AL",
    STATEFP == "28" ~ "MS",
    STATEFP == "22" ~ "LA",
    STATEFP == "05" ~ "AR",
    STATEFP == "47" ~ "TN",
    STATEFP == "37" ~ "NC",
    STATEFP == "45" ~ "SC",
    STATEFP == "21" ~ "KY",
    STATEFP == "54" ~ "WV",
    STATEFP == "51" ~ "VA",
    STATEFP == "48" ~ "TX"))
    

counties_category <- read_csv(here("Data/Processed/counties_category.csv"))


map_countycat <- left_join(SE_Counties, counties_category, by = c(
  "FIPSCNTY","PSTATABB"))

ggplot()+
  geom_sf(data = SE_Counties, aes(color = "Category))
  
