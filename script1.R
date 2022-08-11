GDP <- read.csv("gpd_v2_20220427.csv")

library(tidyverse)

unique(GDP$region)

# testes

GDP %>% 
  filter(region == "")

vazio <- GDP %>% 
  filter(region == "")

unique(vazio$wb_region)

# fim de testes

europe <- GDP %>% 
  filter(region == "Central and Eastern Europe" | region == "Western Europe")

saveRDS(europe, "GDP_europe.rds")






