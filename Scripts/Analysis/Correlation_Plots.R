# Author: Sylvia Hipp
# Purpose: Create plots showing relationship between population, plant category, and response var
# Apr 9, 2026

# Setup Environment -------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(tidycensus)
library(janitor)
library(glue)
library(moments)
library(cowplot)

# Below poverty: percentile ranking

# adult asthma: Age-adjusted prevalence of adults who answer “yes” both to both 
#               of the following questions: “Have you ever been told by a doctor, 
#               nurse, or other health professional that you have asthma?” and the 
#               question “Do you still have asthma?", 2021.

# No High School diploma: percentile ranking

# Riverine Flooding: annualized frequency

## Color Levels
color_levels <- c("Contains FF" = "darkred", 
                  "Contains FF and solar/wind" = "gold", 
                  "Contains solar/wind" = "darkgreen", 
                  "No Plant" = "darkgrey")


# Load Data ---------------------------------------------------------------

counties_category <- read_csv("Data/Processed/counties_category.csv") %>% 
  clean_names()  

cvi_by_county <- readRDS("Data/Processed/cvi_data_by_county_best_cols.rds")

cvi_egrid_df <- left_join(
  cvi_by_county, 
  counties_category %>% select(fipscode, category), 
  by = c("fips_county" = "fipscode")
) %>%
  mutate(category = ifelse(is.na(category), "No Plant", category)) %>% 
  filter(!str_detect(category, "Other")) %>% 
  mutate(category = ordered(category))

summary(cvi_egrid_df)

# make table of distribution of counties across 

  #filter(!str_detect(category, "Other"))


# Wrangle data for plotting -----------------------------------------------

df_to_plot <- cvi_egrid_df %>% 
  pivot_longer(cols = c(current_adult_asthma:riverine_flooding_annualized_frequency_log), 
               names_to = "metric") %>% 
  mutate(metric = case_when(metric == "below_poverty" ~ "Below Poverty (pct)", 
                            metric == "current_adult_asthma" ~ "Adult Asthma Prevalence", 
                            metric == "no_high_school_diploma" ~ "No High School Diploma (pct)", 
                            metric == "riverine_flooding_annualized_frequency_log" ~ "log(Riverine Flooding Freq.)"))

# aggregate means
means <- df_to_plot %>% 
  filter(value != -Inf) %>% 
  group_by(category, metric) %>% 
  summarize(across(c(value, population_log), mean)) %>% 
  ungroup()

# TODO: Maybe update scale? 


# Correlation Plots -------------------------------------------------------

corr_plots <- ggplot(mapping = aes(x = population_log, y = value)) + 
  geom_point(aes(color = category),           # show points for all counties
             data = df_to_plot,
             alpha = 0.4, size = 1) +   # exclude legend for small points
  geom_point(aes(fill = category),            # show points for averages
             data = means,
             size = 5, shape = 21, color = "black") + 
  facet_wrap(vars(metric), 
             scales = "free_y") +
  scale_color_manual(values = color_levels) +
  scale_fill_manual(values = color_levels) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "log(Population)", 
       y = NULL, 
       fill = NULL) + 
  guides(color = "none")

ggsave("Figures/Presentation/scatter_plots.png", corr_plots, 
       height = 4, width = 6, scale = 1.3)

# Population Correlation --------------------------------------------------

# get order of categories by descending population
pop_levels <- means %>%
  arrange(desc(population_log)) %>% 
  distinct(category) %>% 
  pull() %>% as.character()

pop_boxplots <- cvi_egrid_df %>% 
  mutate(category = factor(category, levels = pop_levels)) %>% 
  ggplot(aes(x = category, y = population_log, fill = category)) + 
    geom_boxplot() + 
    scale_fill_manual(values = color_levels) + 
    theme_bw() + 
    theme(axis.text.x = element_text(vjust = 0.5, angle = 20)) + 
    labs(x = NULL, 
         y = "log(Population)")

ggsave("Figures/Presentation/population_boxplots.png", pop_boxplots)

# State Plots -------------------------------------------------------------

# box plots with state on the x axis, order states by the variable? 
# or consistent order based on just one of the variables that has the greatest distinction

