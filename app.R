
#
library(tidyverse)
library(data.table)
library(geojsonio)
library(rgdal)
library(shiny)
library(leaflet)
library(scales)
library(rmapshaper)

data <- read_csv("llsrv_cpue_1985_2018.csv")
###Points to line function
source("points_to_line_function.R")

#Shp files
states <- geojson_json(states, geometry = "polygon", group = "group")
#groundfish <- readOGR(".",layer = "Groundfish_Statistical_Areas_2001") #then read it out
#groundfish <- spTransform(groundfish, CRS("+proj=longlat +datum=WGS84 +no_defs")) #put it into right form
#simpleground <- ms_simplify(groundfish, keep = 0.001)
simpleground <- read_rds(file.path("simpleground.rds")) #a smaller version of the groundfish .shp files that loads quicker than groundfish 

###Data wrangling
#changed column name and type to join with spatial file later
colnames(data)[9] <- "STAT_AREA"
data$STAT_AREA <- as.factor(data$STAT_AREA)

#getting lat longs, number of sablefish for each trip
newdata <- data %>%
  group_by(julian_day, date, year, trip_no, set,Vessel, start_lat, start_lon, end_lat, end_lon, STAT_AREA) %>%
  summarize(sablefish = sum(hooks_sablefish))


# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("SEAK Sablefish Catch per Unit Effort"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("slider1",
                     "Choose a year:",
                     min = 1997,
                     max = 2018,
                     value = 2000,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         # plotOutput("plot1"),
         # plotOutput("plot2"), 
         leafletOutput("map1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # sablefishdata <- reactive(data %>%
  #                           na.omit() %>%
  #                            filter(year == input$slider1))
  x <- reactive(newdata %>%
    filter(year == input$slider1) 
    )
  #Warning: Error in UseMethod: no applicable method for 'filter_' applied to an object of class "c('reactiveExpr', 'reactive')"
  #solved because I waas says x <- reactive(x) ie referring to itself, but now I'm using newdata
  
  #getting polylines ready
  y <- reactive(data.frame(linenumber = c(1:nrow(x()), 1:nrow(x())),
                  lat = c(x()$start_lat, x()$end_lat),
                  long = c(x()$start_lon, x()$end_lon)))
  #vlines2 are polylines to be plotted; aka the transects
  v_lines2 <- reactive(points_to_line(data = y(), 
                             long = "long", 
                             lat = "lat", 
                             id_field = "linenumber"))
  
  ###making chloropleth
  
  
   output$map1 <- renderLeaflet({
     ###Rendering leaflet for only the parts of the map that are not dynamic; ie, the shp files. 
     states %>%
       leaflet()%>%
       addTiles() %>%
       setView(-134.5, 57, 7.5) %>% 
       addPolygons(data = simpleground,
                   #fillColor = ~colors(test@data$total_hooks_sablefish),
                   weight = 2, #make darks darker and lights lighter
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7,
                   popup = paste("STAT Area:", simpleground@data$STAT_AREA, "<br>"
                                 #"Sablefish Surveyed:", simpleground@data$total_hooks_sablefish
                                 )) %>%
       addPolylines(data = v_lines2()) %>%
       addMarkers(clusterOptions = markerClusterOptions(),
                  x()$start_lon,
                  x()$start_lat,
                  popup = paste("Date:", x()$date, "<br>",
                                "Vessel:", x()$Vessel, "<br>",
                                "Set:", x()$set,"<br>",
                                "Sablefish surveyed:",x()$sablefish)
                  )
       
   })
   
   # Incremental changes to the map (in this case, replacing the
   # vertical lines and markers when a new year is chosen) should be performed in
   # an observer. Each independent set of things that can change
   # should be managed in its own observer.
   # observe({
   #   #pal <- colorpal()
   #    
   #   leafletProxy("map1") %>%
   #      clearShapes()
   #     # addPolylines(data = v_lines2())
   # 
   # })
   
   # output$plot1 <- renderPlot({
   #   sablefishdata() %>%
   #     ggplot(aes(x = julian_day, y = hooks_sablefish)) +
   #     geom_bar(stat = "identity", aes(fill = Stat)) +
   #     theme_classic(base_size = 15) + 
   #     scale_x_continuous(breaks = pretty_breaks()) +
   #     scale_color_manual(values = c("red",
   #                                   "green",
   #                                   "blue"))
   # })
   # 
   # output$plot2 <- renderPlot({
   #   data %>%
   #     na.omit() %>%
   #     group_by(year) %>%
   #     summarize(sablefish_surveyed = sum(hooks_sablefish)) %>%
   #     ggplot(aes(x = year, y = sablefish_surveyed)) +
   #     geom_bar(stat = "identity") +
   #     theme_classic(base_size = 15) 
   # })
}

#if i can get it so the Stat area will be highlighted if you run your mouse over the 
# Run the application 
shinyApp(ui = ui, server = server)

