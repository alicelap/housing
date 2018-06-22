library(tidyverse)
library("rgdal")
library(RColorBrewer)
library(sf)
library(cartography)
library(maps)

#  BASE ACS

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/ACS_CBSA")

data_units <- read_csv2("ACS_16_5YR_B25003_with_ann.csv", col_names = T)

colnames(data_units)
data_units <- data_units %>%
  mutate(
    pct_owner_units = HD01_VD02 / HD01_VD01 * 100,
    pct_renter_units = HD01_VD03 / HD01_VD01 * 100,
    total_units = pct_renter_units + pct_owner_units,
    GEOID = str_sub(GEO.id, -5, -1)
  )

data_pop <- read_csv2("ACS_16_5YR_B25033_with_ann.csv", col_names = T)
colnames(data_pop)
data_pop <- data_pop %>%
  select("GEO.id", c(3:7), c(18:19)) %>%
  mutate(
    pct_owner_pop = HD01_VD02 / HD01_VD01 * 100,
    pct_renter_pop = HD01_VD08 / HD01_VD01 * 100,
    total_pop = pct_renter_pop + pct_owner_pop,
    GEOID = str_sub(GEO.id, -5, -1)
  )

# join
data_ACS_CBSA <- left_join(data_pop, data_units, by = c("GEO.id", "GEO.display-label", "GEOID"))

# BASE PSH

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/PSH")
data <- read_csv2("CBSA_2012.csv", col_names = T)

colnames(data)

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
  select(entities, name, code, total_units, pct_occupied, people_per_unit, people_total) %>%
  mutate(
    states = str_sub(entities, -2, -1),
    GEOID = str_sub(entities, 1, 5)
  )

# BASE FINALE

# join of the two data_frames
data_combined_CBSA <- full_join(data_ACS_CBSA, data, by = ("GEOID"))

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
