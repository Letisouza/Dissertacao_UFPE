GDP <- read.csv("gpd_v2_20220427.csv")

library(tidyverse)

unique(GDP$region)

GDP %>% 
  filter(region == "")

vazio <- GDP %>% 
  filter(region == "")

unique(vazio$wb_region)

europe <- GDP %>% 
  filter(region == "Central and Eastern Europe" | region == "Western Europe")

unique(europe$country) # 32 países, 10 não são da UE.