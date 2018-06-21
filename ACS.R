library(tidyverse)
library("rgdal")
library(cartography)

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/ACS")
data_unit <- read.csv("ACS_16_5YR_B25003_with_ann.csv", header=T, sep=";")

colnames(data_unit)

data_unit <- data_unit %>%
  mutate(
    pct_owner_unit = HD01_VD02/HD01_VD01*100, 
    pct_renter_unit= HD01_VD03/HD01_VD01*100, 
    total_unit=pct_renter_unit + pct_owner_unit,
    GEOID = str_sub(GEO.id,-5,-1),
    state = str_sub(GEOID,1,2),
    )

data_pop <- read.csv("ACS_16_5YR_B25026_with_ann.csv", header=T, sep=";")
colnames(data_pop)
data_pop <- data_pop %>%
  select(c(1:7),c(20:21)) %>%
  mutate(
    pct_owner_pop = HD01_VD02/HD01_VD01*100, 
    pct_renter_pop= HD01_VD09/HD01_VD01*100, 
    total_pop =pct_renter_pop + pct_owner_pop,
    GEOID = str_sub(GEO.id,-5,-1),
    state = str_sub(GEOID,1,2),
  )

data_ACS_county <- left_join(data_pop, data_unit, by=c("GEOID", "GEO.id2","state","GEO.id","GEO.display.label"))
