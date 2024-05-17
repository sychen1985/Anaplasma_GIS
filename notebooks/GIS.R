library(readxl)
geo <- read_excel("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/ch3_anaplasma_herd-level.xlsx")
View(geo)

library(readxl)
geo <- read_excel("C:/Users/syche/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/ch3_anaplasma_herd-level.xlsx")
View(geo)



# YouTube -- GIS 101: Mapping data points in R

library(sf)
library(tidyverse)
library(stringr)
library(httr)
library(giscoR)
library(scales)
library(ggplot2)

gis1 <- geo[!(is.na(geo$coordinates)),]

gis <- gis1[, c(4, 10, 13, 14, 15, 18)]
names(gis) <- c("locationID", "county", "coordinates", "lat", "lon", "prevalence")
head(gis)

crsLongLat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
gis_sf <- gis |>
  sf::st_as_sf(coords = c("lon", "lat"), crs= crsLongLat)

ggplot()+geom_sf(data=gis_sf, color="red", fill="red")

us <- giscoR::gisco_get_countries(resolution="1", country="USA") |>
  sf::st_transform(crsLongLat)

us_places <- sf::st_intersection(gis_sf, us)

ggplot()+geom_sf(data=us_places, aes(size=prevalence), color="red", fill="red")+
  scale_size(range= c(0, 5), breaks=scales::pretty_breaks(n=6))


county <- st_read("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/GIS data/CA_county")


# ===== Final map =====
ggplot()+
  geom_sf(data=county)+
  geom_sf(data=us_places, aes(size=prevalence), color="red", fill="red")+
  scale_size(range= c(0, 5), breaks=scales::pretty_breaks(n=6))+
  theme_minimal()





# Label counties but something wrong

# county_label <- us_places |>
#  dplyr::mutate(
#    lon=unlist(map(geometry, 1)),
#    lat=unlist(map(geometry, 2))) |>
#  dplyr::select(county, lon, lat, prevalence) |>
#  dplyr::arrange(desc(prevalence))
# head(county_label)

# ggplot()+
#  geom_sf(data=us_places, aes(size=prevalence), color="red", fill="red")+
#  scale_size(range= c(0, 5), breaks=scales::pretty_breaks(n=6))+
#  ggrepel::geom_text_repel(county_label[1:10, ],
#                           mapping=aes(x=lon, y=lat, label=county),
#                           color="black", fontface="bold")






library(leaflet)
location <- leaflet()%>%
  addTiles()%>%
  addMarkers(lng=geo$lon, lat=geo$lat, popup=geo$LocationID)
location
plot(location)








library(sf)
library(tmap)
library(dplyr)
library(magrittr)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)
library(rna)

county <- st_read("C:/Users/sychen11/Box/Anaplasma projects/Shih-Yu_Dissertation/Dissertation/Chapter 3/Ch3_Analysis/data set/GIS data/CA_county")
plot(county)

tm_shape(county)+tm_borders()+tm_compass(position=c("right", "top")) 


