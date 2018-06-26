library(tidyverse)
library(RColorBrewer)
library("rgdal")
library(sf)

# DATA ACS 

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/ACS")

data_unit <- read_csv2("ACS_16_5YR_B25003_with_ann.csv", col_names=T)
colnames(data_unit)
data_unit <- data_unit %>%
  mutate(
    pct_owner_unit = HD01_VD02/HD01_VD01*100, 
    pct_renter_unit= HD01_VD03/HD01_VD01*100, 
    total_unit=pct_renter_unit + pct_owner_unit,
    GEOID = str_sub(GEO.id,-5,-1),
    state = str_sub(GEOID,1,2)
  )

data_pop <- read_csv2("ACS_16_5YR_B25026_with_ann.csv", col_names=T)
colnames(data_pop)
data_pop <- data_pop %>%
  select(c(1:7),c(20:21)) %>%
  mutate(
    pct_owner_pop = HD01_VD02/HD01_VD01*100, 
    pct_renter_pop= HD01_VD09/HD01_VD01*100, 
    total_pop =pct_renter_pop + pct_owner_pop,
    GEOID = str_sub(GEO.id,-5,-1),
    state = str_sub(GEOID,1,2)
  )

head(data_pop)

data_ACS_county <- left_join(data_pop, data_unit, by=c("GEOID", "GEO.id2","state","GEO.id","GEO.display-label"))

# DATA PSH 

setwd("/Users/Alice/Documents/ENSAE/2A/Warwick/Housing data/PSH")
data <- read_csv2("COUNTY_2012.csv", col_names=T)

data <- data %>%
  filter(program_label == "Public Housing" & !(is.na(total_units))) %>%
  select(states, entities, total_units, pct_occupied, people_per_unit, people_total, state) %>%
  mutate(
    GEOID = str_sub(entities,-5,-1)
    ) %>%
  filter(!(GEOID=="78999") & !(GEOID=="66999")) # removes Guam and VI

# DATA TOTAL 

# join of the two data_frames
data_combined <- full_join(data_ACS_county, data, by =("GEOID"))

# REPRESENTATION GEOGRAPHIQUE

# GGPLOT 

# Fonds de carte
mtq_CBSA <- read_sf(
  dsn = "/Users/Alice/Downloads/tl_2012_us_county", layer = "tl_2012_us_county",
  quiet = TRUE,
  stringsAsFactors = FALSE
)

ggplot() +
  geom_sf(data = mtq_CBSA$geometry) + 
  geom_sf(mapping=aes()data = data$pct_occupied)


# CARTOGRAPHY 

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


