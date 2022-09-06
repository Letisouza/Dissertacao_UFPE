library(tidyverse)

gdp <- readRDS("gdp_europe_final.rds")

per_country <- gdp %>%
  filter(!leader %in% c("Petr Necas","Bohuslav Sobotka", "Andrius Kubilius", "Algirdas Butkevicius", "Beate Szydlo", "Donald Tusk", "Emil Boc", "Viktor Ponta", "Dacian Ciolos")) %>% 
  group_by(country, leader,yearbegin, yearend, lr) %>% 
  summarise(totalaverage = (unique(totalaverage))) %>% 
  arrange(country, yearbegin) %>% 
  filter(yearend >= 2012,
         yearbegin <= 2020)

base <- as.data.frame(per_country)

formalnotice <- c(2,3,3,5,1,3,3,1,4,2,13,0,10,3,2,8,6,10,4,11,9,1,2,2,4,3,7,0,4,6,6,5,8,5,2,1,3,0,0)

base <- cbind(base, formalnotice)

saveRDS(base, "base.rds")

------
  
base <- readRDS("base.rds")  
  
# cases: Hungary and Czechia

ggplot(base,
  aes(formalnotice, totalaverage)) +
  geom_point()
