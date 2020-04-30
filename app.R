
#no applicable method for 'geojson_json' applied to an object of class "c('geofeaturecollection', 'geojson', 'geo_json', 'json')"
#solved by clearing global environment; too much noise
library(tidyverse)
library(data.table)
library(geojsonio)
library(rgdal)
library(shiny)
library(leaflet)
library(scales)
library(rmapshaper)
library(RColorBrewer)

##TO DO: 
#make it so map doesn't redraw every time I change the year

data <- read_csv("llsrv_cpue_1985_2018.csv")
###Points to line function
source("points_to_line_function.R")

#Shp files
states <- geojson_json(states, geometry = "polygon", group = "group")

simpleground <- read_rds(file.path("simpleground.rds")) #a smaller version of the groundfish .shp files that loads quicker than groundfish 

###Data wrangling
#changed column name and type to join with spatial file later
colnames(data)[9] <- "STAT_AREA"
data$STAT_AREA <- as.factor(data$STAT_AREA)

#getting lat longs, number of sablefish for each trip
newdata <- data %>%
  group_by(julian_day, date, year, trip_no, set,Vessel, start_lat, start_lon, end_lat, end_lon, STAT_AREA) %>%
  summarize(sablefish = sum(hooks_sablefish))

#getting catch per State area
areacatch <- newdata #duplicate data 


areacatch$STAT_AREA <- as.factor(areacatch$STAT_AREA)
#data$STAT_AREA <- as.factor(data$STAT_AREA)
#changing NA sablefish surveyed to 0; ok but a bit imprecise
#areacatch[is.na(areacatch["sablefish"])] <- 0
areacatch <- areacatch %>%
  mutate(sablefish = if_else(is.na(sablefish), 0, sablefish)) %>%
  group_by(STAT_AREA, year) %>%
  summarise(total_hooks_sablefish = sum(sablefish))


ui <- navbarPage("SEAK Sablefish Catch per Unit Effort", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer"),
                    
           
           leafletOutput("map1", width = "100%", height = 1000),
          
           
           
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                         sliderInput("slider1",
                                     "Choose a year:",
                                     min = 1997,
                                     max = 2018,
                                     value = 2015,
                                     sep = ""),
                         
                         selectInput("colors", "Polygon Color Scheme", selected = "YlGn",
                                     rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
                         
           )
           ),
           tabPanel("Data Explorer",
                    plotOutput("plot1")
                    
                    )
           
        )


# Define server logic
server <- function(input, output) {
   
  #x now has every sampling trip for a selected year
  x <- reactive(newdata %>%
    filter(year == input$slider1) 
    )
  #Warning: Error in UseMethod: no applicable method for 'filter_' applied to an object of class "c('reactiveExpr', 'reactive')"
  #solved because I waas says x <- reactive(x) ie referring to itself, but now I'm using newdata
  
  #getting polylines ready by preparing data to feed into pointstoline() function
  y <- reactive(data.frame(linenumber = c(1:nrow(x()), 1:nrow(x())),
                  lat = c(x()$start_lat, x()$end_lat),
                  long = c(x()$start_lon, x()$end_lon)))
  #vlines2 are polylines to be plotted; aka the transects
  #points_to_line() ensures that the points won't connect to each other when plotted
  v_lines2 <- reactive(points_to_line(data = y(), 
                             long = "long", 
                             lat = "lat", 
                             id_field = "linenumber"))
  
  #need to filter for year before doing a full join, otherwise the total_sablefish value for polygon 345731 will get assigned to 345706 polygon (no idea why)
  areacatch_ <- reactive(areacatch %>%
                           filter(year == input$slider1 )) #| is.na(year))
  
  # Warning: Error in : evaluation nested too deeply: infinite recursion / options(expressions=)?
  #   Error in mapply(function(call, srcref) { : 
  #       zero-length inputs cannot be mixed with those of non-zero length
  
  #solved by changing variable names. You always need to rename your reactive data something different than the data inside the reactive expression
  areacatch2 <- reactive(full_join(simpleground@data,
                         areacatch_(),
                         by = "STAT_AREA"))
  
  

  #Color pallette selection
  colorpal <- reactive({
    colorNumeric(input$colors, areacatch2()$total_hooks_sablefish) #use a inputted color pallette based off total hooks
  })
 
  
  ###plotting map

   output$map1 <- renderLeaflet({
     ###Rendering leaflet  
     states %>%
       leaflet()%>%
       addTiles() %>%
       setView(-134.5, 57, 7.5) 
   })
   
   # Incremental changes to the map (in this case, replacing the
   # vertical lines and markers when a new year is chosen) should be performed in
   # an observer. Each independent set of things that can change
   # should be managed in its own observer.
   
   # dynamically change color scheme to what you want and it won't redraw the whole map, only the colors will change 
   
   observe({
     pal <- colorpal()
      
     leafletProxy("map1") %>%
        clearShapes() %>%
        addPolygons(data = simpleground,
                    fillColor = ~pal(areacatch2()$total_hooks_sablefish),
                    #weight = 2, #make darks darker and lights lighter
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    popup = paste("STAT Area:", simpleground@data$STAT_AREA, "<br>",
                                  "Sablefish Surveyed:", areacatch2()$total_hooks_sablefish
                    )) %>%
       addPolylines(data = v_lines2()) #puts polylines on top of the polygons
       
       


   })
  
   
   observe({
  #this has its own observe instance because they're two different inputs you can change right now: slider1 and colors
    #this makes it so that each time you change the year, map1 won't redraw and you can stay zoomed in and exploring the data
     leafletProxy("map1") %>%
       #clearing markers 
       clearMarkerClusters() %>%
       addMarkers(clusterOptions = markerClusterOptions(),
                  x()$start_lon,
                  x()$start_lat,
                  popup = paste("Date:", x()$date, "<br>",
                                "Vessel:", x()$Vessel, "<br>",
                                "Set:", x()$set,"<br>",
                                "Sablefish surveyed:",x()$sablefish)
                  #color = ~x()$Vessel
                  )
   })
   
   
   #plot in the data explore tab
   output$plot1 <- renderPlot({
     areacatch2() %>%
       filter(STAT_AREA == c(345731) | STAT_AREA == 345701 | STAT_AREA == 345631 | STAT_AREA == 345603) %>%
       ggplot(aes(x = STAT_AREA, y = total_hooks_sablefish)) +
       geom_bar(stat = "identity") + #could try and fill by vessel; could be better displaying x() instead of areacatch
       ggtitle(paste(as.character(input$slider1), "Sablefish Hooks by STAT Area")) + 
       xlab("STAT Area") + ylab("Total Number of Sablefish Hooks") +
       theme_classic(base_size = 15) 
     #Warning: Error in +: invalid argument to unary operator
     #solved because 
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

