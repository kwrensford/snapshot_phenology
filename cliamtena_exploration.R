#Cliamte NA Exploration

library(tidyverse)

snapshot_climate <- vroom("data/snapshot_climatedata.csv")

#Select phenology relevant variables
phenology_climate <- snapshot_climate %>%
  select(Year, ID1, Latitude, Longitude, Elevation, MAP, MAT, bFFP, eFFP, FFP, DD5, PAS, CMD)

ggplot(
  phenology_climate,
  aes(x = Year,
      y = CMD,
      group = ID1,
      color = Latitude)
) + 
  geom_line() + 
  scale_color_viridis_c()

ggplot(
  phenology_climate,
  aes(
  x = Longitude,
  y = Latitude,
  color = FFP
  )
) + 
  geom_point(size = 2) + 
  scale_color_viridis_c() + 
  facet_wrap(~Year)
  
