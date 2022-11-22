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
ref <- c("AUT 1","AUT 2", "AUT 3", "AUT 4", "BGR 1", "BGR 2", "BGR 3", "HRV 1", "HRV 2","CZE 1", "CZE 2", "FRA 1", "FRA 2", "FRA 3", "DEU 1", "DEU 2", "GRC 1", "GRC 2", "HUN 1", "HUN 2", "ITA 1", "ITA 2", "LVA 1", "LVA 2", "LVA 3", "LTU 1", "LTU 2", "NLD 1", "NLD 2", "POL 1", "POL 2", "ROU 1", "ROU 2", "SVK 1", "SVK 2", "SWE 1", "SWE 2", "GBR 1", "GBR 2")

base <- cbind(base, formalnotice, ref)

saveRDS(base, "base.rds")

base <- readRDS("base.rds")  

# EXCLUIR titulo: Relação entre Populismo e Infrações a Leis sobre Migração na UE

theme_set(theme_bw())

g1 <- ggplot(base,
             aes(formalnotice, totalaverage, label=country)) +
  geom_jitter(width = 0.14)+
  theme(panel.grid = element_blank()) +
  # theme_classic() +
  scale_y_continuous(breaks=seq(0, 1.2, by = .10)) +
  scale_x_continuous(breaks=seq(0, 15, by = 2)) +
 # geom_text(aes(label=ifelse(totalaverage == 0.875 & formalnotice == 4 & country == "Hungary", "Hungria 1",
  #                           ifelse(totalaverage == 0.825 & formalnotice == 11 & country == "Hungary", "Hungria 2",
   #                                 ifelse(totalaverage == 1.000 & formalnotice == 2 & country == "Czechia", "R. Tcheca 1",
    #                                       ifelse(totalaverage == 0.150 & formalnotice == 13 & country == "Czechia","R. Tcheca 2",''))))),size = 3.5, hjust=0.65,vjust=-0.4) +
  geom_text(aes(label = ref), size = 2.8, hjust=0.65,vjust=-0.4) +
  labs(y="Nível de Populismo", 
       x="Processos por Infração",
       caption = "Fonte: GPD e UE") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9))

g1

ggsave(g1, filename = "g1.jpeg", width = 8, height = 5)

# cases: Hungary and Czechia

base %>% 
  filter(country == "Hungary"|
         country == "Czechia")

