library(tidyverse)

#############
gdp <- readRDS("gdp_europe_final.rds")

per_country <- gdp %>%
  filter(!leader %in% c("Petr Necas","Bohuslav Sobotka", "Andrius Kubilius", "Algirdas Butkevicius", "Beate Szydlo", 
                        "Donald Tusk", "Emil Boc", "Viktor Ponta", "Dacian Ciolos")) %>% 
  group_by(country, leader,yearbegin, yearend, lr, party) %>% 
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

##############

base <- readRDS("base.rds")  

# graph: populism x infractions

theme_set(theme_bw())

g1 <- ggplot(base,
             aes(formalnotice, totalaverage)) +
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
  labs(y="Level of Populism", 
       x="Infringement Procedure",
       caption = "Source: GPD and EU") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9))

g1

ggsave(g1, filename = "g1.jpeg", width = 8, height = 5)


#######

library(readxl)

migration1 <- read_xlsx("migration1.xlsx")

# transforming to a data.frame

df1 <- data.frame(matrix(unlist(migration1), nrow=length(migration1), byrow=T),stringsAsFactors=F)

# turning into a long form

df2 <- as.data.frame(t(df1))

# changing the type of the variables to numeric

df2$V4 <- as.numeric(df2$V4)
df2$V5 <- as.numeric(df2$V5)
df2$V6 <- as.numeric(df2$V6)
df2$V7 <- as.numeric(df2$V7)


# changing the rows with "present" to 2022, even tho it doesn't mean this year is really 
# the end of the term. 

library(dplyr)

migration2 <- df2 %>% 
  mutate_at(vars(V5, V7), funs(round(., 1))) %>% 
  rename("country" = V1,
         "ref" = V2,
         "year" = V3,
         "migr_stock" = V4,
         "migr_stock_pop_perc" = V5,
         "refugee_stock" = V6,
         "refugee_stock_migr_perc" = V7)

saveRDS(migration2, "migration.rds")

############

# graph: migration

migration <- readRDS("migration.rds")

theme_set(theme_bw())

g2 <- ggplot(migration, aes(ref, migr_stock_pop_perc, fill = year)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(breaks=seq(0, 20, by = 5)) +
  labs(y="Immigrant Stock (% of population) ", 
       x="",
       fill = "",
       caption = "Source: UN DESA") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9)) +
  scale_fill_manual(breaks = c("2015", "2019", "2020"),
                    values = c("blue3", "darkgoldenrod1", "bisque4"))

g2

ggsave(g2, filename = "g2.jpeg", width = 10, height = 5)

g3 <- ggplot(migration, aes(ref, refugee_stock_migr_perc, fill = year)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(breaks=seq(0, 20, by = 5)) +
  labs(y="Refugee Stock (% of immigrants) ", 
       x="",
       fill = "",
       caption = "Source: UN DESA") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9)) +
  scale_fill_manual(breaks = c("2015", "2019", "2020"),
                    values = c("blue3", "darkgoldenrod1", "bisque4"))

g3

ggsave(g3, filename = "g3.jpeg", width = 10, height = 5)


#########  
# Ideology

# graph: populism x infractions x ideology

theme_set(theme_bw())

g4 <- ggplot(base,
             aes(formalnotice, totalaverage, color = lr)) +
  geom_jitter(width = 0.14)+
  theme(panel.grid = element_blank()) +
  scale_y_continuous(breaks=seq(0, 1.2, by = .10)) +
  scale_x_continuous(breaks=seq(0, 15, by = 2)) +
  geom_text(aes(label = ref), size = 2.8, hjust=0.65,vjust=-0.4) +
  labs(y="Level of Populism", 
       x="Infringement Procedure",
       color = "Ideology",
       caption = "Source: GPD and EU") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9)) +
  scale_color_manual(labels = c("Left", "Center", "Right"), values = c("blue3", "darkgoldenrod1", "bisque4"))
        

g4

ggsave(g4, filename = "g4.jpeg", width = 8, height = 5)



base <- base %>% 
  mutate(party = ifelse(party == "Unity", "The New Era Party/Unity", 
                        ifelse(party == "Liep?ja Party", "Liepaja Party", 
                               ifelse(party == "Democratic Liberal Party", "National Liberal Party", party))))



ideology <- base %>% 
  filter(lr == 1 & party != "Independent") %>% 
  mutate(ext_r = c(6.9, 6.1, 6.5, 6.1, 6.9, 7.2, 6.1, 5.8, 7.2, 7.9, 8.3, 6.9, 6.9, 5.8, 7.8, 7.9, 5.7, 7.5, 7.1, 7.4, 7.0, 7.1))



theme_set(theme_bw())

g5 <- ggplot(ideology,
             aes(ext_r, totalaverage)) +
  geom_jitter()+
  theme(panel.grid = element_blank()) +
  scale_y_continuous(breaks=seq(0, 1.2, by = .10)) +
  scale_x_continuous(breaks=seq(5.0, 10.0, by = 1.0)) +
  geom_text(aes(label = ref), size = 2.8, hjust=0.65,vjust=-0.4) +
  labs(y="Level of Populism", 
       x="Right Wing",
       caption = "Source: GPD and CHES") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9))


g5


ggsave(g5, filename = "g5.jpeg", width = 8, height = 5)



theme_set(theme_bw())

g6 <- ggplot(ideology,
             aes(ext_r, formalnotice)) +
  geom_jitter(height = 0.12, width = 0.01)+
  theme(panel.grid = element_blank()) +
  scale_y_continuous(breaks=seq(0, 15, by = 2)) +
  scale_x_continuous(breaks=seq(5.0, 10.0, by = 1.0)) +
  geom_text(aes(label = ref), size = 2.9, hjust= 0.5,vjust=-0.1) +
  labs(y="Infringement Procedure", 
       x="Right Wing",
       caption = "Source: GPD and CHES") +
  theme(axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9))


g6


ggsave(g6, filename = "g6.jpeg", width = 8, height = 5)





########

# cases: Hungary and Czechia

base %>% 
  filter(country == "Hungary"|
           country == "Czechia")