library(tidyverse)

gdp_europe <- readRDS("GDP_europe.rds")

unique(gdp_europe$country) # 32 countries, 11 aren't members of EU (Albania, Belarus, North Macedonia, Moldova, Montenegro, Russia, Serbia, Switzerland, UK, Ukraine, Norway)

# EU: Austria, Bulgaria, Croatia, Czechia, Estonia, Finland, France, Germany,Greece,
# Hungary, Ireland, Italy, Latvia, Lithuania, Netherlands, Poland, Slovakia,
# Slovenia, Romania, Spain, Sweden.

# Lacking from EU: Belgium, Cyprus, Denmark, Luxembourg, Malta and Portugal.

unique(gdp_europe$yearbegin) #1990
unique(gdp_europe$yearend) #2021/present(2022)

gdp_europe_selected <- gdp_europe %>% 
  select(country, leader, party, lr, president, term, yearbegin, yearend, speechtype, totalaverage) %>% 
  filter(!country %in% c("Albania", "Belarus", "North Macedonia", "Moldova", "Montenegro", "Norway", "Russia", "Serbia", "Switzerland", "Ukraine"))

unique(gdp_europe_selected$country)

saveRDS(gdp_europe_selected, "gdp_europe_selected.rds")

# UK stays in the EU until 2020.

u <- gdp_europe_selected %>% 
  filter(country == "UK")
