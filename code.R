library(tidyverse)
library(data.table)
library(geojsonio)
library(leaflet)
library(rgdal)
library(rmapshaper)
data <- read_csv("llsrv_cpue_1985_2018.csv")

states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"
states <- geojson_read(states.url, what = "sp")

states_json <- geojson_json(states, geometry = "polygon", group = "group")
states %>%
  leaflet() %>%
  addTiles()

#shp files
#directory <- here::here() #first get the folder path where everything is stored
groundfish <- readOGR(".",layer = "Groundfish_Statistical_Areas_2001") #then read it out
groundfish <- spTransform(groundfish, CRS("+proj=longlat +datum=WGS84 +no_defs")) #put it into right form

#map and dataset have STAT potentially the same 
states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-159, 61, 3.2) %>%
  addPolygons(data = groundfish,
              popup = paste(groundfish@data$REGISTRATI,"\n", groundfish@data$DISTRICT_N),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                    bringToFront = TRUE))

  # addMarkers(clusterOptions = markerClusterOptions(),
  #            layerId = ~hobogps$site,
  #            lat = ~hobogps$Latitude,
  #            lng = ~hobogps$Longitude,
  #            popup = paste("Site: ", hobogps$site, "<br>"))

###Data
x <- data %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(sablefish_surveyed = sum(hooks_sablefish)) %>%
  ggplot(aes(x = year, y = sablefish_surveyed)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 15) 


View(x)

x %>%
  ggplot(aes(x = year, y = sablefish_surveyed)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 15) 

#work on incorporating stat into visualization
