# Author: Sylvia Hipp
# Purpose: Aggregate CVI data at the county level, weighted by population
# Mar 25, 2026

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

# Rail crossings: Proximity to an at-grade railroad crossing  (5km centroid radius), 2021.

# Load CVI Data -----------------------------------------------------------

southern_states <- c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", 
                     "KY", "WV", "VA", "TX")

columnlist <- c("fips_code",
                "state", 
                "current_adult_asthma",
                #"rail_crossings",
                "below_poverty",
                "no_high_school_diploma",
                "riverine_flooding_annualized_frequency") 

cvi_data <- read_excel(path=here('Data','Raw','cvi',"CVI_subset.xlsx"),
                       skip = 0,
                       sheet="Indicator Native Units",
                       col_names=T) %>%
  clean_names() %>% 
  filter(state %in% southern_states) %>% 
  select(columnlist)


# Get population at the census tract level --------------------------------

census_tract_pop <- get_acs(geography = "tract",
                            state = southern_states, 
                            variables = "B01003_001", 
                            geometry = FALSE) %>% 
  #mutate(county_name = str_match(NAME, ";\\s*([^;]+ County);\\s*([^;]+)")[,2]) %>% 
  select("fips_code" = GEOID, "population" = estimate)

county_pop <- get_acs(geography = "county",
                      state = southern_states, 
                      variables = "B01003_001", 
                      geometry = FALSE) %>% 
  select("fips_code" = GEOID, "county_name" = NAME, "population" = estimate)

# Join population and clean CVI data --------------------------------------

not_in_census <- cvi_data %>% anti_join(census_tract_pop)  # 5376
not_in_cvi <- census_tract_pop %>% anti_join(cvi_data)     # 11068

# ASSUMPTION: for tracts that do not have population, assign a population of 1 

cvi_data_pop <- cvi_data %>% 
  left_join(census_tract_pop, by = "fips_code") %>% 
  mutate(population = replace_na(population, 1))
  
# cvi_data_pop %>% filter(population == 1) %>% group_by(state) %>% summarize(n())


# Aggregate CVI Data to county level --------------------------------------

cvi_county <- cvi_data_pop %>% 
  mutate(fips_county = str_extract(fips_code,"^\\d{5}")) %>% 
  group_by(state, fips_county) %>% 
# summarize tract data to weighted average, weighted by population 
  summarize(across(c("current_adult_asthma":"riverine_flooding_annualized_frequency"),
                   ~sum(population*., na.rm = TRUE)/sum(population, na.rm = TRUE))) %>% 
  ungroup() %>% 
# add on county-level population data
# exclude counties without population data in the census
  inner_join(county_pop, by = c("fips_county" = "fips_code"))


# Investigate Variable Distribution ---------------------------------------

metrics <- cvi_county %>% select(-c(state:fips_county, county_name)) %>% names()

# visualize distributions
plot_distrib <- function(metric, county_df, include_skew=TRUE){
  df <- county_df %>% 
    select(fips_county, all_of(metric))
  
  metric_data <- df %>% select(all_of(metric)) %>% pull()
  
  skewness <- skewness(metric_data %>% subset(.!=-Inf), na.rm = TRUE)
  kurtosis <- kurtosis(metric_data %>% subset(.!=-Inf), na.rm = TRUE) 
  
  plot <- ggplot(df, aes(x = !!sym(metric))) + 
    geom_histogram(fill = "grey", color = "darkgrey") + 
    theme_bw() 
  
  if(include_skew){
    plot <- plot + labs(caption = glue("Skewness: {skewness}\nKurtosis: {kurtosis}"))
  }
  return(list(plot, skewness))
}

plots <- map(metrics, plot_distrib, cvi_county)
names(plots) <- metrics

plots

# skewed_vars <- c("rail_crossings", "below_poverty", "no_high_school_diploma", 
#                  "riverine_flooding_annualized_frequency", "population")
  
# Log Adjust Variables ----------------------------------------------------

cvi_county_log_adj <- cvi_county %>%
  select(fips_county, all_of(metrics)) %>% 
  mutate(across(c(-fips_county), log))   # can maybe arbitrarily add 0.1

plots_log <- map(metrics, plot_distrib, cvi_county_log_adj)
names(plots_log) <- metrics
plots_log

cvi_county_final <- left_join(
  cvi_county, 
  cvi_county_log_adj, 
  by = "fips_county", 
  suffix = c("", "_log")
) %>% 
  relocate(county_name, .after = "fips_county")


# check whether original or log-transformed variable is better
best_col <- c()
for (metric in metrics){
  orig_skew <- plots[metric][[1]][[2]]
  log_skew <- plots_log[metric][[1]][[2]]
  
  if((abs(orig_skew) < abs(log_skew)) | (abs(orig_skew)<0.5)){
    best_col <- best_col %>% append(metric)
  }
  else{
    best_col <- best_col %>% append(str_c(metric, "log", sep = "_"))
  }
  print(c(metric, orig_skew, log_skew))
}
#best_col

cvi_county_best_cols <- cvi_county_final %>% 
  select(state:county_name, all_of(best_col))

#write_rds(cvi_county_final, "Data/Processed/cvi_data_by_county.rds")
#write_rds(cvi_county_best_cols, "Data/Processed/cvi_data_by_county_best_cols.rds")

# Visualization -----------------------------------------------------------

# plot grid of metrics
plots_best_cols <- map(best_col, plot_distrib, cvi_county_best_cols, FALSE)
plots_best_cols <- lapply(plots_best_cols, function(x) x[[1]])  # extract just the plot

title <- ggdraw() +
  draw_label("Distribution of Response Variables and Population", 
             fontface = 'bold', size = 14, hjust = 0.5)

final_distributions <- plot_grid(
  title, 
  plot_grid(plotlist = plots_best_cols, nrow = 3), 
  nrow = 2, rel_heights = c(0.1, 0.9)
) + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
  
ggsave("Figures/Presentation/variable_distributions.png", final_distributions, 
       width = 7, height = 5, scale = 1.15)

# ggplot(cvi_county_final) + 
#   geom_boxplot(aes)
