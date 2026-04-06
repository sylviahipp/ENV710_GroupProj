library(tidyverse)
library(here)

cvi_egrid_join <- read.csv("Data/Processed/cvi_egrid_join.csv")

colnames(cvi_egrid_join)

#ANOVA

#asthma
asthma_ANOVA <- aov(current_adult_asthma ~ Category, data = cvi_egrid_join)
summary(asthma_ANOVA)

TukeyHSD(asthma_ANOVA)
