library(tidyverse)

gdp <- readRDS("gdp_europe_final.rds")

per_country <- gdp %>%
  filter(!leader %in% c("Petr Necas","Bohuslav Sobotka", "Andrius Kubilius", "Algirdas Butkevicius", "Beate Szydlo", 
                        "Donald Tusk", "Emil Boc", "Viktor Ponta", "Dacian Ciolos")) %>% 
  group_by(country, leader,yearbegin, yearend, lr) %>% 
  summarise(totalaverage = (unique(totalaverage))) %>% 
  arrange(country, yearbegin) %>% 
  filter(yearend >= 2012,
         yearbegin <= 2020)

# These leaders took off are coded both as president or prime-minister along another leaders in the same years in the same country. 
# I made the decision to choose the leaders who cover most time during the time of analysis, since the leader isn't important here.

base <- as.data.frame(per_country)

formalnotice <- c(2,3,3,5,1,3,3,1,4,2,13,0,10,3,2,8,6,10,4,11,9,1,2,2,4,3,7,0,4,6,6,5,8,5,2,1,3,0,0)

base <- cbind(base, formalnotice)

saveRDS(base, "base.rds")

------
  
  base <- readRDS("base.rds")  

# cases: Hungary and Czechia

g1 <- ggplot(base,
             aes(formalnotice, totalaverage, label=country)) +
  geom_jitter(width = 0.08)+
  theme_classic()+
  scale_y_continuous(breaks=seq(0, 1, by = .10)) +
  scale_x_continuous(breaks=seq(0, 15, by = 2)) +
  labs(title="Relação entre Populismo e Infrações a \nLeis sobre Migração na UE", 
       y="Nível de Populismo", 
       x="Procedimentos de Infração",
       caption = "Fonte: GDP e UE") +
  geom_text(aes(label=ifelse(totalaverage > 0.8 | formalnotice > 10, country,'')),hjust=0.6,vjust=-0.4)

g1
