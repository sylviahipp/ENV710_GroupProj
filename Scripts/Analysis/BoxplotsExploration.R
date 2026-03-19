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

#-SUBSET CVI DATA ----------
southern_states <- c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", 
                     "KY", "WV", "VA", "TX", "DC")

columnlist <- c("current_adult_asthma",
                "rail_crossings",
                "below_poverty",
                "no_high_school_diploma",
                "riverine_flooding_annualized_frequency",
                "fips_code",
                "state")

aggregatecvi <- cvi_data %>% 
  janitor::clean_names() %>% 
  filter(state %in% southern_states) %>% 
  select(columnlist) %>% 
  mutate(fips_county = str_extract(fips_code,"^\\d{5}")) %>% 
  group_by(fips_county, state) %>%
  summarize(across(c("current_adult_asthma":"riverine_flooding_annualized_frequency"),
                   ~mean(.,na.rm=T)))

write.csv(aggregatecvi, file = "Data/Raw/cvi/aggregatecvi.csv")

#-Join CVI & eGRID classification-----------

counties_categories <- read.csv(here('Data','Processed','counties_category.csv'))

cvi_county_join <- left_join(x = aggregatecvi, y=counties_cat, by = c("fips_county" = "FIPSCODE"))

clean_joined <- cvi_county_join %>% 
  select(!PSTATABB) %>% 
  mutate(Category = replace_when(x = Category, is.na(Category) ~ "Neither" ))

write.csv(x = clean_joined, file = "Data/Processed/CleanedCVIandeGRID.csv")
  

#-BOXPLOTS------------------

adultasthma <- ggplot(data = clean_joined, aes(x = Category, y = current_adult_asthma, fill = Category))+
  geom_boxplot()+
  labs(title = "Adult Asthma Rates in Southeastern Counties \nby Power Plant Category",
       y = "Current Adult Asthma Rate")

railcrossings <- ggplot(data = clean_joined, aes(x = Category, y = rail_crossings, fill = Category))+
  geom_boxplot()+
  labs(title = "Count of Railroad Crossings in Southeastern Counties \nby Power Plant Category",
       y = "Current Adult Asthma Rate")

belowpoverty <- ggplot(data = clean_joined, aes(x = Category, y = below_poverty, fill = Category))+
  geom_boxplot()+
  labs(title = "Count of Railroad Crossings in Southeastern Counties \nby Power Plant Category",
       y = "Current Adult Asthma Rate")

nohsdiploma <- ggplot(data = clean_joined, aes(x = Category, y = no_high_school_diploma, fill = Category))+
  geom_boxplot()+
  labs(title = "Rate of Residents w/o High School Diploma in Southeastern Counties \nby Power Plant Category",
       y = "Current Adult Asthma Rate")

riverineflooding <- ggplot(data = clean_joined, aes(x = Category, y = riverine_flooding_annualized_frequency, fill = Category))+
  geom_boxplot()+
  labs(title = "Annualized Frequency of Riverine Flooding in Southeastern Counties \nby Power Plant Category",
       y = "Current Adult Asthma Rate")

ggsave(filename = "Scripts/Analysis/Figures/AdultAsthmaplot.jpg",plot = adultasthma, device = "jpeg")
ggsave(filename = "Scripts/Analysis/Figures/RailCrossingsplot.jpg",plot = railcrossings, device = "jpeg")
ggsave(filename = "Scripts/Analysis/Figures/BelowPovertyplot.jpg",plot = belowpoverty, device = "jpeg")
ggsave(filename = "Scripts/Analysis/Figures/NoHSDiplomaplot.jpg",plot = nohsdiploma, device = "jpeg")
ggsave(filename = "Scripts/Analysis/Figures/RiverineFloodingplot.jpg",plot = riverineflooding, device = "jpeg")
