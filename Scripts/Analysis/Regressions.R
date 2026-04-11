# Author: Josh Salzberg
# Purpose: Regression Analysis 
# March 18, 2026


#-Setup & Packages------------- 

librarylist <- c("openxlsx", "readxl",'ggplot2','moments', 'stats','here', 'tidyverse', 'nlme','naniar','DHARMa','gtsummary','broom')
lapply(librarylist, require, character.only = T)
here()
data <- read.csv(here('Data','Processed','cvi_egrid_join.csv'))
summary(data)

data1 <- data %>% 
  filter(riverine_flooding_annualized_frequency_log != -Inf) %>% 
  mutate(category_GLM = case_when(Category == "Contains FF" ~ 1,
                                   Category == "Contains solar/wind" ~ 1,
                                   Category == "Contains FF and solar/wind" ~ 1,
                                   T ~ 0 )) #Plant exists = 1; No Plant = 0



#-Logistic Regressions-------------
CategoryGLM1 <- glm(category_GLM ~ population_log,
                 data = data1,
                 family = "binomial")

CategoryGLM2 <- glm(category_GLM ~ current_adult_asthma,
                    data = data1,
                    family = "binomial")

CategoryGLM3 <- glm(category_GLM ~ below_poverty,
                    data = data1,
                    family = "binomial")

CategoryGLM4 <- glm(category_GLM ~ no_high_school_diploma,
                    data = data1,
                    family = "binomial")

CategoryGLM5 <- glm(category_GLM ~ riverine_flooding_annualized_frequency_log,
                    data = data1,
                    family = "binomial")

level1AIC <- as.data.frame(rbind(AIC(CategoryGLM1),AIC(CategoryGLM2),AIC(CategoryGLM3),AIC(CategoryGLM4),AIC(CategoryGLM5)))

# best performing model is GLM2 - using this to guide how
# we add a layer of complexity

CategoryGLM21 <- glm(category_GLM ~ current_adult_asthma+ population_log,
                    data = data1,
                    family = "binomial")

CategoryGLM23 <- glm(category_GLM ~ current_adult_asthma + below_poverty,
                    data = data1,
                    family = "binomial")
CategoryGLM24 <- glm(category_GLM ~ current_adult_asthma + no_high_school_diploma,
                     data = data1,
                     family = "binomial")
CategoryGLM25 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log,
                     data = data1,
                     family = "binomial")


level2AIC <- as.data.frame(rbind(AIC(CategoryGLM21),
                                 AIC(CategoryGLM23),
                                 AIC(CategoryGLM24),
                                 AIC(CategoryGLM25)))
min(level2AIC) # GLM25 (current_adult_asthma + riverine_flooding_annualized_frequency_log) has best AIC 
# using this to narrow choices

# level 3 complexity
CategoryGLM251 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + population_log,
                      data = data1,
                      family = "binomial")

CategoryGLM253 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + below_poverty,
                      data = data1,
                      family = "binomial")

CategoryGLM254 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + no_high_school_diploma,
                      data = data1,
                      family = "binomial")

level3AIC <- as.data.frame(rbind(AIC(CategoryGLM251),
                                 AIC(CategoryGLM253),
                                 AIC(CategoryGLM254)))
min(level3AIC) # an improvement to add population_log

# level 4 complexity
CategoryGLM2513 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + 
                         population_log + below_poverty,
                       data = data1,
                       family = "binomial")
CategoryGLM2514 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + 
                         population_log + no_high_school_diploma,
                       data = data1,
                       family = "binomial")

level4AIC <- as.data.frame(rbind(AIC(CategoryGLM2513),
                                 AIC(CategoryGLM2514)))
min(level4AIC) # fractional improvement to add no_high_school_diploma

# test if all explanatory variables together improve model any further:
# CategoryGLM25143 <- glm(category_GLM ~ current_adult_asthma + riverine_flooding_annualized_frequency_log + 
#                          population_log + no_high_school_diploma + below_poverty,
#                        data = data1,
#                        family = "binomial")
# level5AIC <- as.data.frame(rbind(AIC(CategoryGLM25143)))
# level5AIC < min(level4AIC) # RETURNS FALSE


print(CategoryGLM2514)
summary(CategoryGLM2514)
simulateResiduals(CategoryGLM2514) %>% plot()
simulateResiduals(CategoryGLM251) %>% plot()

# 
# 
# #-Linear Regressions-------------
# asthma_lm <- lm(current_adult_asthma ~ (Category + population_log), data =  data)
# summary(asthma_lm)
# plot(asthma_lm)
# 
# poverty_lm <- lm(below_poverty ~ (Category + population_log), data =  data)
# summary(poverty_lm)
# plot(poverty_lm)
# 
# 
# no_hs_lm <- lm(no_high_school_diploma ~ (Category + population_log), data =  data)
# summary(no_hs_lm)
# plot(no_hs_lm)
# 
# flooding_lm <- lm(riverine_flooding_annualized_frequency_log ~ (Category + population_log), data =  data)
# summary(flooding_lm)
# plot(flooding_lm)
# 
# 
# 
# #-Linear Mixed Effects Regressions---------
# asthmasthmasthma_lme <- lme(current_adult_asthma ~ Category + population_log, random =~1|state, data =  data)
# summary(asthma_lme)
# plot(asthma_lme)
# qqnorm(asthma_lme)
# 
# poverty_lme <- lme(below_poverty ~ Category + population_log, random =~1|state, data =  data)
# summary(poverty_lme)
# plot(poverty_lme)
# qqnorm(poverty_lme)
# 
# diploma_lme <- lme(no_high_school_diploma ~ Category + population_log, random =~1|state, data =  data)
# summary(diploma_lme)
# plot(diploma_lme)
# qqnorm(diploma_lme)
# 
# flooding_lme <- lme(riverine_flooding_annualized_frequency_log ~ Category + population_log, random =~1|state, data =  data)
# summary(flooding_lme)
# plot(flooding_lme)
# qqnorm(flooding_lme)
# 
# cor.test(data$Category, data$population_log, data$riverine_flooding_annualized_frequency_log)
