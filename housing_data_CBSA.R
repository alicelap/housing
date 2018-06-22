library(tidyverse)
library(openxlsx)
library(cartography)
library("rgdal")

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/PSH")
data <- read.csv("CBSA_2012.csv", header = T, sep = ";")
# data <- read_csv2("CBSA_2012.csv", col_names=T)

colnames(data)   

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
  select(entities, name, code, total_units, pct_occupied, people_per_unit, people_total) %>%
  mutate(
    states = str_sub(entities,-2,-1),
    GEOID = str_sub(entities,1,5)
    )

# join of the two data_frames
data_combined_CBSA <- full_join(data_ACS_CBSA, data, by =("GEOID"))

# Fonds de carte 
mtq_CBSA <- readOGR(dsn ="/Users/Alice/Downloads/tl_2012_us_cbsa",layer = "tl_2012_us_cbsa")

cols <- carto.pal(pal1 = "green.pal", # first color gradient
                  n1 = 3, # number of colors in the first gradiant
                  pal2 = "red.pal", # second color gradient
                  n2 = 2) # number of colors in the second gradiant

# Impression du fond de carte
plot(mtq_CBSA, border = "black",lwd = 0.5)

choroLayer(spdf = mtq_CBSA, # SpatialPolygonsDataFrame des communes
           spdfid ="GEOID",
           df = data, # data frame qui contient la variable
           dfid = "GEOID",     
           var = "pct_occupied", # la variable que l'on représente
           col = cols, # colors 
           breaks = c(50,60,70,80,90,100),
           border = "grey20", # color of the polygons borders
           lwd = 0.0005, # width of the borders
           legend.pos = "bottomleft", # position of the legend
           legend.title.txt = "Title legend", # title of the legend
           legend.values.rnd = 2, # number of decimal in the legend values
           add = TRUE) # add the layer to the current plot


