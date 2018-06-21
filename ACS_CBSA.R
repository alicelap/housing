library(tidyverse)

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/ACS_CBSA")

data_units <- read.csv("ACS_16_5YR_B25003_with_ann.csv", header=T, sep=";")
colnames(data_units)
data_units <- data_units %>%
  select(-"GEO.id2") %>%
  mutate(
    pct_owner_units = HD01_VD02/HD01_VD01*100, 
    pct_renter_units= HD01_VD03/HD01_VD01*100, 
    total_units=pct_renter_units + pct_owner_units,
    GEOID = str_sub(GEO.id,-5,-1)
  ) 

data_pop <- read.csv("ACS_16_5YR_B25033_with_ann.csv", header=T, sep=";")
colnames(data_pop)
data_pop <- data_pop %>%
  select("GEO.id",c(3:7),c(20:21)) %>%
  mutate(
    pct_owner_pop = HD01_VD02/HD01_VD01*100, 
    pct_renter_pop= HD01_VD09/HD01_VD01*100, 
    total_pop=pct_renter_pop + pct_owner_pop,
    GEOID = str_sub(GEO.id,-5,-1)
  )

# join
data_ACS_CBSA <- left_join(data_pop, data_units, by=c("GEO.id","GEO.display.label", "GEOID"))
