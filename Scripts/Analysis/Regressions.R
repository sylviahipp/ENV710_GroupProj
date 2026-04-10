# Author: Josh Salzberg
# Purpose: Regression Analysis 
# March 18, 2026


#-Setup & Packages------------- 

librarylist <- c("openxlsx", "readxl",'ggplot2','moments', 'stats','here', 'tidyverse', 'nlme')
lapply(librarylist, require, character.only = T)
here()
data <- read.csv(here('Data','Processed','cvi_egrid_join.csv'))
summary(data)


#-Linear Regressions-------------
asthma_lm <- lm(current_adult_asthma ~ (Category + population), data =  data)
summary(asthma_lm)
plot(asthma_lm)

rail_lm <- lm(rail_crossings ~ (Category + population), data =  data)
summary(rail_lm)
plot(rail_lm)

poverty_lm <- lm(below_poverty ~ (Category + population), data =  data)
summary(poverty_lm)
plot(poverty_lm)


no_hs_lm <- lm(no_high_school_diploma ~ (Category + population), data =  data)
summary(no_hs_lm)
plot(no_hs_lm)

flooding_lm <- lm(riverine_flooding_annualized_frequency ~ (Category + population), data =  data)
summary(flooding_lm)
plot(flooding_lm)

#-Linear Mixed Effects Regressions---------
asthmasthmasthma_lme1 <- lme(current_adult_asthma ~ Category + population, random =~1|state, data =  data)
summary(asthma_lme)
plot(asthma_lme)
qqnorm(asthma_lme)

rail_lme <- lme(rail_crossings ~ Category + population, random =~1|state, data =  data)
summary(rail_lme)
plot(rail_lme)
qqnorm(rail_lme)

poverty_lme <- lme(below_poverty ~ Category + population, random =~1|state, data =  data)
summary(poverty_lme)
plot(poverty_lme)
qqnorm(poverty_lme)

diploma_lme <- lme(no_high_school_diploma ~ Category + population, random =~1|state, data =  data)
summary(diploma_lme)
plot(diploma_lme)
qqnorm(diploma_lme)

flooding_lme <- lme(riverine_flooding_annualized_frequency ~ Category + population, random =~1|state, data =  data)
summary(flooding_lme)
plot(flooding_lme)
qqnorm(flooding_lme)

