library(tidyverse)
library(openxlsx)
library("rgdal")
library(cartography)

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/PSH")
data <- read.csv("COUNTY_2012.csv", header = T, sep = ";")

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
# & !(states=="PR Puerto Rico") & !(states == "AK Alaska")) 
  select(states, entities, total_units, pct_occupied, people_per_unit, people_total, state) %>%
  mutate(
    GEO.id2 = str_sub(entities,-5,-1),
    GEO.id2 = ifelse(str_sub(GEO.id2,1,1)==0, str_sub(GEO.id2,2,5),str_sub(GEO.id2,1,5))
    ) %>%
  filter()

# join of the two data_frames
data_combined <- full_join(data_ACS_county, data, by =("GEO.id2"))


data_combined %>%
  filter(is.na(HD01_VD01.x)) # NA sur GU Guam et VI Virgin Islands

data_combined %>%
  mutate(population_totale = sum(HD01_VD01.x, na.rm = T)) 
# population totale = pop US hors PR et AK

sub_sample <- data_combined %>%
  filter(is.na(pct_occupied)) %>%
  mutate(pop_no_info = sum(HD01_VD01.x))
# pop manquante dans PSH = 13,7% de la pop totale

mtq_county <- readOGR(dsn ="/Users/Alice/Downloads/tl_2012_us_county",layer = "tl_2012_us_county")


