library(tidyverse)
library(data.table)
library(geojsonio)
library(leaflet)
library(rgdal)
library(rmapshaper)

data <- read_csv("llsrv_cpue_1985_2018.csv")
source("points_to_line_function.R")

# states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"
# states <- geojson_read(states.url, what = "sp")

states_json <- geojson_json(states, geometry = "polygon", group = "group")
# states %>%
#   leaflet() %>%
#   addTiles()
###READING IN shp files
#directory <- here::here() #first get the folder path where everything is stored
groundfish <- readOGR(".",layer = "Groundfish_Statistical_Areas_2001") #then read it out
groundfish <- spTransform(groundfish, CRS("+proj=longlat +datum=WGS84 +no_defs")) #put it into right form

###THIS is awesome; reduces data from 83 mb to 9.6, way easier to load and navigate around
simpleground <- ms_simplify(groundfish, keep = 0.001) #might be worth saving these as shape files and doing away with other ones because they're so much smaller
#trying to save 
write_rds(simpleground, path = file.path(".", "simpleground.rds"))
simpleground <- read_rds(file.path("simpleground.rds"))

states %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = sg_saved, 
              popup = paste("STAT Area:", sg_saved@data$STAT_AREA))

###PLOTTING SAMPLING TRANSECTS
#gets each trip for each vessel for each day in a given year
colnames(data)[9] <- "STAT_AREA"
#this dataset can be used for plotting th individual sampling trips and their data
x <- data %>%
  #filter(year == 2006) %>%
  group_by(julian_day, date, year, trip_no, set,Vessel, start_lat, start_lon, end_lat, end_lon, STAT_AREA) %>%
  summarize(sablefish = sum(hooks_sablefish))
x <- x %>%
  filter(year == 2006)
#puts data in format for points_to_line function  
print(nrow(x))
y <- data.frame(linenumber = c(1:nrow(x), 1:nrow(x)),
                    lat = c(x$start_lat, x$end_lat),
                    long = c(x$start_lon, x$end_lon),
                year = c(x$year,x$year))
# mydf3 <- head(mydf2)
# mydf3 <- mydf3 %>%
#   mutate(group = c(1,2,3,4,5,6)) #c("A", "A", "B","B", "C","C")
# y <- y %>%
#   filter(year == 2006)
v_lines3 <- points_to_line(data = y, 
                          long = "long", 
                          lat = "lat", 
                          id_field = "linenumber")
states %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(v_lines3)
##Combining datasets to make chloropleth
###makign chlorpleth
#first, make data right types to combine data with groundfish spatial data
xx <- x #duplicate data 
test <- simpleground
colnames(data)[9] <- "STAT_AREA"
#columns we want to add to spatial dataframe
# selected <- data %>%
#   select(year, julian_day, STAT_AREA, start_lat, start_lon, end_lat, end_lon,hooks_sablefish)

xx$STAT_AREA <- as.factor(xx$STAT_AREA)
#data$STAT_AREA <- as.factor(data$STAT_AREA)
#changing NA sablefish surveyed to 0; ok but a bit imprecise
#xx[is.na(xx["sablefish"])] <- 0
xx <- xx %>%
  mutate(sablefish = if_else(is.na(sablefish), 0, sablefish)) %>%
  group_by(STAT_AREA) %>%
  summarise(total_hooks_sablefish = sum(sablefish))
#full join retains all rows (stat area included) and districts without surveys have NA for total sablefish
test@data <- full_join(test@data,
                       xx,
                       by = "STAT_AREA")
#View(test@data)

#determining bin sizes for sablefish

test@data %>%
  ggplot(aes(x = total_hooks_sablefish)) +
  geom_histogram(bins = 100)

bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, Inf)#what are the cutoffs for coloring these? anything between these ranges will be one color or another color
colors <-colorBin(palette = "YlGn", #whats the gradient I want
                  domain = test@data$total_hooks_sablefish, #this is the thing I'm basing my color gradient off of
                  bins = bins)

# states %>%
#   leaflet() %>%
#   addTiles() %>%
#   setView(-159, 61, 3.2) %>%
#   addPolygons(data = test,
#               fillColor = ~colors(test@data$hooks_sablefish),
#               weight = 2, #make darks darker and lights lighter
#               opacity = 1,
#               color = "white", 
#               dashArray = "3",
#               fillOpacity = 0.7,
#               popup = paste(test@data$REGISTRATI,"<br>", test@data$DISTRICT_N, "<br>", "Stat area:", test@data$STAT_AREA,"<br>", "Sablefish:", test@data$hooks_sablefish),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE))

# test@data <- test@data %>%
#   filter(year == 2002,
#          !is.na(start_lat))

states %>%
  leaflet()%>%
  addTiles() %>%
  setView(-134.5, 57, 7.5) %>% 
  addMarkers(clusterOptions = markerClusterOptions(),
             ~x$start_lon,
             ~x$start_lat, 
             popup = paste("Date:", x$date, "<br>",
                           "Vessel:", x$Vessel, "<br>",
                           "Set:", x$set,"<br>",
                           "Sablefish surveyed:",x$sablefish)) %>%
  addPolygons(data = test,
              fillColor = ~colors(test@data$total_hooks_sablefish),
              weight = 2, #make darks darker and lights lighter
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste("STAT Area:", test@data$STAT_AREA, "<br>", 
                            "Sablefish Surveyed:", test@data$total_hooks_sablefish)) %>%
  addPolylines(data = v_lines2)

#next: make summary of total sablefish surveyed in each stat_area then have it display as part of the popup, then make it part of the chloropleth
  
#addMarkers(~x$start_lat, ~x$start_lon) %>%

#test <- simpleground
#map and dataset have STAT potentially the same 
# states %>%
#   leaflet() %>%
#   addTiles() %>%
#   setView(-159, 61, 3.2) %>%
#   addPolygons(data = simpleground,
#               popup = paste(simpleground@data$REGISTRATI,"<br>", simpleground@data$DISTRICT_N),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                     bringToFront = TRUE))


map <- states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-159, 61, 3.2) %>%
  addPolygons(data = test,
              fillColor = ~colors(test@data$hooks_sablefish),
              weight = 2, #make darks darker and lights lighter
              opacity = 1,
              color = "white", 
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste(test@data$REGISTRATI,"<br>", test@data$DISTRICT_N, "<br>", "Stat area:", test@data$STAT_AREA,"<br>", "Sablefish:", test@data$hooks_sablefish),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

#try putting start/stop lat/longs in new df to 
# 
# lat_longs <- selected %>%
#   select(-year, -julian_day, -hooks_sablefish, -STAT_AREA)

# map <- map %>% 
#   addMarkers(~test@data$start_lon,~test@data$start_lat, popup=~test@data$julian_day)
# map
# for(i in 1:nrow(test@data)){
#   map <- addPolylines(map, lat = as.numeric(test@data[i, c(2, 4)]), 
#                        lng = as.numeric(test@data[i, c(3, 5)]))
# }
# map
# #
###need to potentially make each stat_area into one larger section. Or get a more precise way of shwoing where hooks_sablefish occur.
#the issue now is that when the datasets are joined, the sablefish numbers are assigned to any old stat_area that matches, instead of near where they might actually have been found. 

  # addMarkers(clusterOptions = markerClusterOptions(),
  #            layerId = ~hobogps$site,
  #            lat = ~hobogps$Latitude,
  #            lng = ~hobogps$Longitude,
  #            popup = paste("Site: ", hobogps$site, "<br>"))

###Data
x <- data %>%
  na.omit() %>%
  group_by(year, STAT_AREA) %>%
  summarize(sablefish_surveyed = sum(hooks_sablefish)) %>%
  ggplot(aes(x = year, y = sablefish_surveyed)) +
  geom_bar(stat = "identity", aes(fill = STAT_AREA)) +
  theme_classic(base_size = 15) 


View(x)
x
x %>%
  ggplot(aes(x = year, y = sablefish_surveyed)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 15) 

#work on incorporating stat into visualization
#renaming column name in prep for joining
colnames(data)[9] <- "STAT_AREA"
selected <- data %>%
  select(year, julian_day, STAT_AREA, hooks_sablefish)

test <- simpleground
#this is fucking up the columns
#for some reason when this column is converted to numeric, all the values change. Easier to convert other df$stat_Area to factor before joinging
#test@data$STAT_AREA <- as.numeric(test@data$STAT_AREA) #change stat_area_type from factor first
selected$STAT_AREA <- as.factor(selected$STAT_AREA)
#leftr join throws out rows in second dataset not found in first dataset
y <- left_join(test@data,
               selected,
               by = "STAT_AREA")
y <- y %>%
  select(year,julian_day,STAT_AREA,REGISTRATI,DISTRICT_N,hooks_sablefish, Shape__Are, Shape__Len)
y %>%
  filter(!is.na(hooks_sablefish) & REGISTRATI != "<NA>")

groundfish@data[groundfish@data$STAT_AREA,]
y[y$STAT_AREA == 125,]

data[data$STAT_AREA==345731,]
intersect(simpleground@data$STAT_AREA,selected$STAT_AREA) #there are 5 values where stat_area is the same for both datasets

simpleground@data$STAT_AREA <- transform(simpleground@data$STAT_AREA, as.numeric(simpleground@data$STAT_AREA))

class(simpleground@data$STAT_AREA)
