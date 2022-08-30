library(tidyverse)

# Cleaning and converting base -----------------------------------------------------------------------


gdp <- readRDS("gdp_europe_selected.rds")

# transforming to a data.frame

df1 <- data.frame(matrix(unlist(gdp), nrow=length(gdp), byrow=T),stringsAsFactors=F)

# turning into a long form

df2 <- as.data.frame(t(df1))

# changing the type of the yearend variable to numeric

df2$V8 <- as.numeric(df2$V8)

# changing the rows with "present" to 2022, even tho it doesn't mean this year is really 
# the end of the term. 

gdp_europe_changed <- df2 %>% 
  mutate(V8 = ifelse(is.na(V8) == T, 2022, V8)) %>% 
  rename("country" = V1,
         "leader" = V2,
         "party" = V3,
         "lr" = V4,
         "president" = V5,
         "term" = V6,
         "yearbegin" = V7,
         "yearend" = V8,
         "speechtype" = V9,
         "totalaverage" = V10)

saveRDS(gdp_europe_changed, "gdp_europe_changed.rds")


# Exploring --------------------------------------------

gdp_europe <- readRDS("gdp_europe_changed.rds")

per_country <- gdp_europe %>% 
  group_by(country, leader, yearend, lr) %>% 
  summarise(unique(totalaverage)) 
 
# Estonia, Finland, Ireland, Slovenia and Spain don't have data up to 2019 or close enough (same leader analyzed few years before)

# EU: Austria, Bulgaria, Croatia, Czechia, France, Germany,Greece,
# Hungary, Italy, Latvia, Lithuania, Netherlands, Norway, Poland, Slovakia,
# Romania, Sweden.

# Lacking: Belgium, Cyprus, Denmark, Luxembourg, Malta - no data -, Estonia, Finland, 
# Ireland, Slovenia and Spain - not enough data.

gdp_europe_final <- gdp_europe %>% 
  select(country, leader, party, lr, president, term, yearbegin, yearend, speechtype, totalaverage) %>% 
  filter(!country %in% c("Estonia","Finland", "Ireland","Slovenia", "Spain"))

saveRDS(gdp_europe_final, "gdp_europe_final.rds")
