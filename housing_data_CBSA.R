library(tidyverse)
library("rgdal")
library(RColorBrewer)
library(sf)
library(cartography)
library(maps)

#  BASE ACS

setwd("/Users/Alice/University of Warwick/Rathelot, Roland - CB_IL/Alice/housing/data/")

data_unit <- read_csv2("ACS_16_5YR_B25003_CBSA.csv", col_names = T)

colnames(data_units)
data_unit <- data_unit %>%
  select(-c(starts_with("HD02_"))) %>%
  rename(
    total_units = HD01_VD01,
    total_units_owner = HD01_VD02,
    total_units_renter = HD01_VD03
  ) %>%
  mutate(
    pct_owner_unit = total_units_owner/total_units*100, 
    pct_renter_unit = total_units_renter/total_units*100, 
    pct_tot_unit = pct_renter_unit + pct_owner_unit,
    GEOID = str_sub(GEO.id,-5,-1)
  )

head(data_unit)

data_pop <- read_csv2("ACS_16_5YR_B25033_CBSA.csv", col_names = T)
colnames(data_pop)
data_pop <- data_pop %>%
  select(c(1:7),c(18), -c(starts_with("HD02_"))) %>%
  rename(
    total_pop = HD01_VD01,
    total_pop_owner = HD01_VD02,
    total_pop_renter = HD01_VD08
  ) %>%
  mutate(
    pct_owner_pop = total_pop_owner/total_pop*100, 
    pct_renter_pop= total_pop_renter/total_pop*100, 
    pct_tot_pop =pct_renter_pop + pct_owner_pop,
    GEOID = str_sub(GEO.id,-5,-1)
  )

head(data_pop)

# join
data_ACS_CBSA <- left_join(data_pop, data_unit, by = c("GEO.id", "GEO.display-label", "GEOID"))

# BASE PSH

data <- read_csv2("CBSA_2012.csv", col_names = T)

colnames(data)

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
  select(entities, total_units, pct_occupied, people_total) %>%
  rename(
    total_units_ph = total_units, 
    pct_occupied_ph=pct_occupied, 
    total_pop_ph = people_total
  ) %>%
  mutate(GEOID = str_sub(entities,1,5)) %>%
  filter(!(GEOID=="78999") & !(GEOID=="66999")) # removes Guam and VI

colSums(is.na(data))

# BASE FINALE

# join of the two data_frames
data_combined_CBSA <- full_join(data_ACS_CBSA, data, by =("GEOID"))

data_combined_CBSA <- data_combined_CBSA %>% select(-c("pct_tot_pop","pct_tot_unit"))

colnames(data_combined_CBSA)
data_combined_CBSA <- data_combined_CBSA[,c(9,1:3,15,4:8,18,10:14,16:17)]

# A FAIRE :
colSums(is.na(data_combined_CBSA)) 
# 220 CBSA présents dans ACS mais pas dans PSH
# 85 CBSA présents dans PSH mais pas ACS 
# pourquoi ? 

# REPRESENTATION GEOGRAPHIQUE  

# Fonds de carte
mtq_CBSA <- read_sf(
  dsn = "/Users/Alice/Downloads/tl_2012_us_cbsa", layer = "tl_2012_us_cbsa",
  quiet = TRUE,
  stringsAsFactors = FALSE
)

cols <- carto.pal(
  pal1 = "green.pal", # first color gradient
  n1 = 3, # number of colors in the first gradiant
  pal2 = "red.pal", # second color gradient
  n2 = 2 # number of colors in the second gradiant
) 

# Impression du fond de carte
plot(mtq_CBSA$geometry, border = "black", lwd = 0.5)

ggplot() +
  geom_sf(data = mtq_CBSA$geometry) + 
  geom_sf(mapping=aes()data = data$pct_occupied)

choroLayer(
  spdf = mtq_CBSA, # SpatialPolygonsDataFrame des CBSA
  spdfid = "GEOID",
  df = data, # data frame qui contient la variable
  dfid = "GEOID",
  var = "pct_occupied", # la variable que l'on représente
  col = cols, # colors
  breaks = c(50, 60, 70, 80, 90, 100),
  border = "grey20", # color of the polygons borders
  lwd = 0.0005, # width of the borders
  legend.pos = "bottomleft", 
  legend.title.txt = "Title legend", 
  legend.values.rnd = 2, # number of decimal in the legend values
  add = TRUE
) # add the layer to the current plot
