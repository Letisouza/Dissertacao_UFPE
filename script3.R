library(tidyverse)

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

per_country <- gdp_europe_changed %>% 
  group_by(country, leader, yearend, lr) %>% 
  summarise(unique(totalaverage))
 
