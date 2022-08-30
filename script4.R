library(tidyverse)

gdp <- readRDS("gdp_europe_final.rds")

per_country <- gdp %>% 
  group_by(country, leader, yearend, lr) %>% 
  summarise(unique(totalaverage))
