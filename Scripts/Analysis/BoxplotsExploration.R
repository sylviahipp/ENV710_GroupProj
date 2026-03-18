# Author: Josh Salzberg
# Purpose: Explore Distributions of Key Variables of Southeastern US counties by plant category
# March 18, 2026


#-Setup & Packages-------------

librarylist <- c("openxlsx", "readxl",'ggplot2','moments', 'stats','here', 'tidyverse')
lapply(librarylist, require, character.only = T)
here()
# install.packages('janitor')

#-Import Data---------------

cvi_data <- read_excel(
  path=here('Data','Raw','cvi',"CVI_subset.xlsx"),
  skip = 0,
  sheet="Indicator Native Units",
  col_names=T) 

# - SUBSET DATA ----------
southern_states <- c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", 
                     "KY", "WV", "VA", "TX", "DC")

columnlist <- c("current_adult_asthma",
                "rail_crossings",
                "below_poverty",
                "no_high_school_diploma",
                "riverine_flooding_annualized_frequency",
                "fips_code",
                "state")

subsetcvi <- cvi_data %>% 
  janitor::clean_names() %>% 
  filter(state %in% southern_states) %>% 
  select(columnlist) %>% 
  mutate(fips_county = str_extract(fips_code,"^\\d{5}")) %>% 
  group_by(fips_county, state) %>%
  summarize(across(c("current_adult_asthma":"riverine_flooding_annualized_frequency"),
                   ~mean(.,na.rm=T)))
