library(tidyverse)
library(here)

cvi_egrid_join <- read.csv("Data/Processed/cvi_egrid_join.csv")

colnames(cvi_egrid_join)

#ANOVA

#asthma
asthma_ANOVA <- aov(current_adult_asthma ~ Category, data = cvi_egrid_join)
summary(asthma_ANOVA)
TukeyHSD(asthma_ANOVA)

#population
pop_ANOVA <- aov(population ~ Category, data = cvi_egrid_join)
summary(pop_ANOVA)
TukeyHSD(pop_ANOVA)

#railroad
rail_ANOVA <- aov(rail_crossings ~ Category, data = cvi_egrid_join)
summary(rail_ANOVA)
TukeyHSD(rail_ANOVA)

#poverty
poverty_ANOVA <- aov(below_poverty ~ Category, data = cvi_egrid_join)
summary(poverty_ANOVA)
TukeyHSD(poverty_ANOVA)

#hs diploma
hs_ANOVA <- aov(no_high_school_diploma ~ Category, data = cvi_egrid_join)
summary(hs_ANOVA)
TukeyHSD(hs_ANOVA)

#riverine_flooding_annualized_frequency
riverine_ANOVA <- aov(riverine_flooding_annualized_frequency ~ Category, data = cvi_egrid_join)
summary(riverine_ANOVA)
TukeyHSD(riverine_ANOVA)

