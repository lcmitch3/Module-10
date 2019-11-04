# Clear workspace, install, and load packages ----
rm(list=ls(all=TRUE))

library(tidyverse)
library(sf)
library(ggspatial)

#read in data files
Emergency_Shelters <- read_sf("data/spatial_data/Potential_Emergency_Shelters.shp")
Urban_zones <- read_sf("data/spatial_data/NCDOT_Smoothed_Urban_Boundaries_simple.shp")
state_bound <- read_sf("data/spatial_data/state_bounds.shp")
nc_county <- read_sf("data/spatial_data/NCDOT_County_Boundaries.shp")

#inspect data
head(state_bound)
head(Emergency_Shelters)
head(Urban_zones)
head(nc_county)

# re-project all data to EPSG = 32119 (NAD83 for NC)
# https://epsg.io/
Emergency_Shelters <- st_transform(Emergency_Shelters, 32119)
Urban_zones <- st_transform(Urban_zones, 32119)
state_bound <- st_transform(state_bound, 32119)
nc_county <- st_transform(nc_county,32119)

#select only North Carolina from the state_bound data.
state_bound %>% 
  filter(NAME == "North Carolina") -> nc

#combine emergency shelters with urban zones using intersection 
Urban_shelters <- st_intersection(Urban_zones, Emergency_Shelters)

shelters <- 
  Urban_shelters %>%
  group_by(TYPE)%>%
  count() 
#How many potential emergency shelter sites are located in urban areas across NC? 1199

#single out just the area Capital Area Metropolitan Organization
Capital <- 
  Urban_zones %>%
  filter(NAME == "Capital Area Metroplitan Organization")
  
#buffer around this area by 50 km 
#since units are in meters use 50000
Cap_buff <- st_buffer(Capital, 50000)

#Emergency shelters that intersect with the buffer
Cap_area_shelters <- st_intersection(Cap_buff, Emergency_Shelters)

#How many shelters are located within 50 km of the Capital Area Metropolitan Organication? 556

ggplot()+
  geom_sf(data = Cap_buff, fill = "red", alpha = .4, color = "red")+
  geom_sf(data = Capital)+
  geom_sf(data = Cap_area_shelters,size = 2, alpha = .5)+
  theme_bw()

nc_county %>%
  st_join(Emergency_Shelters) -> County_shelter

county <-
  County_shelter %>%
  group_by(CountyName)%>%
  count()

ggplot()+
  geom_sf(data = county, aes(color = n))+
  scale_color_viridis_c()+
  theme_bw()


