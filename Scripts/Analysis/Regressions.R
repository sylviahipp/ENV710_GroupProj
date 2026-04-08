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
asthma_lm_interactions1 <- lm(current_adult_asthma ~ (Category*state) + population, data =  data1)
asthma_lm_interactions2 <- lm(current_adult_asthma ~ (Category) + (population*state), data =  data1)
asthma_lm_interactions3 <- lm(current_adult_asthma ~ (Category + population)*state, data =  data1)
summary(asthma_lm)
plot(asthma_lm)
summary(asthma_lm_interactions1)
df_asthma_lm_comparison <- (data.frame(AIC(asthma_lm), AIC(asthma_lm_interactions1), AIC(asthma_lm_interactions2), AIC(asthma_lm_interactions3)))
df_asthma_lm_comparison

rail_lm <- lm(rail_crossings ~ (Category + population), data =  data1)
rail_lm_interaction1 <- lm(rail_crossings ~ (Category*state) + (population), data =  data1)
rail_lm_interaction2 <- lm(rail_crossings ~ (Category) + (population*state), data =  data1)
rail_lm_interaction3 <- lm(rail_crossings ~ (Category + population)*state, data =  data1)
summary(rail_lm)
plot(rail_lm)
df_rail_lm_comparison <- data.frame(AIC(rail_lm), AIC(rail_lm_interaction1), AIC(rail_lm_interaction2), AIC(rail_lm_interaction3))

poverty_lm <- lm(below_poverty ~ (Category + population), data =  data1)
poverty_lm_interaction1 <- lm(below_poverty ~ (Category*state) + (population), data =  data1)
poverty_lm_interaction2 <- lm(below_poverty ~ (Category) + (population*state), data =  data1)
poverty_lm_interaction3 <- lm(below_poverty ~ (Category + population)*state, data =  data1)
summary(poverty_lm)
plot(poverty_lm)
df_poverty_lm_comparison <- data.frame(AIC(poverty_lm), AIC(poverty_lm_interaction1), AIC(poverty_lm_interaction2), AIC(poverty_lm_interaction3))

no_hs_lm <- lm(no_high_school_diploma ~ (Category + population), data =  data1)
no_hs_lm_interaction1 <- lm(no_high_school_diploma ~ (Category*state) + (population), data =  data1)
no_hs_lm_interaction2 <- lm(no_high_school_diploma ~ (Category) + (population*state), data =  data1)
no_hs_lm_interaction3 <- lm(no_high_school_diploma ~ (Category + population)*state, data =  data1)
summary(no_hs_lm)
plot(no_hs_lm)
df_noHS_lm_comparison <- data.frame(AIC(no_hs_lm),AIC(no_hs_lm_interaction1),AIC(no_hs_lm_interaction2),AIC(no_hs_lm_interaction3))

flooding_lm <- lm(riverine_flooding_annualized_frequency ~ (Category + population), data =  data1)
flooding_lm_interaction1 <- lm(riverine_flooding_annualized_frequency ~ (Category*state) + (population), data =  data1)
flooding_lm_interaction2 <- lm(riverine_flooding_annualized_frequency ~ (Category) + (population*state), data =  data1)
flooding_lm_interaction3 <- lm(riverine_flooding_annualized_frequency ~ (Category + population)*state, data =  data1)
summary(flooding_lm)
plot(flooding_lm)
df_flooding_lm_comparison <- data.frame(AIC(flooding_lm),AIC(flooding_lm_interaction1),AIC(flooding_lm_interaction2),AIC(flooding_lm_interaction3))

## Linear Mixed Effects Regressions
asthmasthmasthma_lme <- lme(current_adult_asthma ~ Category + population, random =~1|state, data =  data1)
summary(asthma_lme)
plot(asthma_lme)
qqnorm(asthma_lme)

rail_lme <- lme(rail_crossings ~ Category + population, random =~1|state, data =  data1)
summary(rail_lme)
plot(rail_lme)
qqnorm(rail_lme)

poverty_lme <- lme(below_poverty ~ Category + population, random =~1|state, data =  data1)
summary(poverty_lme)
plot(poverty_lme)
qqnorm(poverty_lme)

diploma_lme <- lme(no_high_school_diploma ~ Category + population, random =~1|state, data =  data1)
summary(diploma_lme)
plot(diploma_lme)
qqnorm(diploma_lme)

flooding_lme <- lme(riverine_flooding_annualized_frequency ~ Category + population, random =~1|state, data =  data1)
summary(flooding_lme)
plot(flooding_lme)
qqnorm(flooding_lme)
