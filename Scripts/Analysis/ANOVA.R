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






