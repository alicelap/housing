library(tidyverse)
library(openxlsx)
library("rgdal")
library(cartography)
library(plyr)
library(RColorBrewer)

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/PSH")
data <- read.csv("COUNTY_2012.csv", header = T, sep = ";")

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
  select(states, entities, total_units, pct_occupied, people_per_unit, people_total, state) %>%
  mutate(
    GEOID = str_sub(entities,-5,-1)
    ) %>%
  filter(!(GEOID=="78999") & !(GEOID=="66999")) # removes Guam and VI

# join of the two data_frames
data_combined <- full_join(data_ACS_county, data, by =("GEOID"))

# Fonds de carte 
mtq_county <- readOGR(dsn ="/Users/Alice/Downloads/tl_2012_us_county",layer = "tl_2012_us_county")

cols <- carto.pal(pal1 = "green.pal", # first color gradient
                  n1 = 3, # number of colors in the first gradiant
                  pal2 = "red.pal", # second color gradient
                  n2 = 2) # number of colors in the second gradiant

# Impression du fond de carte
plot(mtq_county, border = "black",lwd = 0.5)

choroLayer(spdf = mtq_county, # SpatialPolygonsDataFrame des communes
           spdfid ="GEOID",
           df = data, # data frame qui contient la variable
           dfid = "GEOID",     
           var = "pct_occupied", # la variable que l'on représente
           breaks = c(0,20,40,60,80,100), # list of breaks
           col = cols, # colors 
           border = "grey20", # color of the polygons borders
           lwd = 0.0005, # width of the borders
           legend.pos = "bottomleft", # position of the legend
           legend.title.txt = "Grands logements", # title of the legend
           legend.values.rnd = 2, # number of decimal in the legend values
           add = TRUE) # add the layer to the current plot


