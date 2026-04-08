# Author: Josh Salzberg
# Purpose: Regression Analysis 
# March 18, 2026


#-Setup & Packages------------- 

librarylist <- c("openxlsx", "readxl",'ggplot2','moments', 'stats','here', 'tidyverse', 'nlme')
lapply(librarylist, require, character.only = T)
here()
data <- read.csv(here('Data','Processed','cvi_egrid_join.csv'))
summary(data)

data1 <- data %>% 
  filter(!is.na(population),
         Category!="Other plant")

#-Regressions-----------------
## Linear Regressions
asthma_lm <- lm(current_adult_asthma ~ (Category + population), data =  data1)
summary(asthma_lm)
plot(asthma_lm)

rail_lm <- lm(rail_crossings ~ (Category + population)*state, data =  data1)
summary(rail_lm)
plot(rail_lm)

poverty_lm <- lm(below_poverty ~ (Category + population), data =  data1)
summary(poverty_lm)
plot(poverty_lm)

no_hs_lm <- lm(no_high_school_diploma ~ (Category + population)*state, data =  data1)
summary(no_hs_lm)
plot(no_hs_lm)

flooding_lm <- lm(riverine_flooding_annualized_frequency ~ (Category + population)*state, data =  data1)
summary(flooding_lm)
plot(flooding_lm)

## Linear Mixed Effects Regressions
asthmasthmasthma_lme <- lme(current_adult_asthma ~ Category + population, random =~1|state, data =  data1)
summary(asthma_lme)
plot(asthma_lme)

rail_lme <- lme(rail_crossings ~ Category + population, random =~1|state, data =  data1)
summary(rail_lme)
plot(rail_lme)

poverty_lme <- lme(below_poverty ~ Category + population, random =~1|state, data =  data1)
summary(poverty_lme)

diploma_lme <- lme(no_high_school_diploma ~ Category + population, random =~1|state, data =  data1)
summary(diploma_lme)

flooding_lme <- lme(riverine_flooding_annualized_frequency ~ Category + population, random =~1|state, data =  data1)
summary(flooding_lme)
