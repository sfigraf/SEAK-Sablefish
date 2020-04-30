
#
library(tidyverse)
library(data.table)
library(geojsonio)
library(rgdal)
library(shiny)
library(leaflet)
library(scales)
library(rmapshaper)
library(RColorBrewer)

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

#getting catch per State area
areacatch <- newdata #duplicate data 

#colnames(data)[9] <- "STAT_AREA"
#columns we want to add to spatial dataframe
# selected <- data %>%
#   select(year, julian_day, STAT_AREA, start_lat, start_lon, end_lat, end_lon,hooks_sablefish)

areacatch$STAT_AREA <- as.factor(areacatch$STAT_AREA)
#data$STAT_AREA <- as.factor(data$STAT_AREA)
#changing NA sablefish surveyed to 0; ok but a bit imprecise
#areacatch[is.na(areacatch["sablefish"])] <- 0
areacatch <- areacatch %>%
  mutate(sablefish = if_else(is.na(sablefish), 0, sablefish)) %>%
  group_by(STAT_AREA, year) %>%
  summarise(total_hooks_sablefish = sum(sablefish))

areacatch <- full_join(simpleground@data,
                   areacatch,
                   by = "STAT_AREA")

# xxx <- areacatch %>%
#   filter(year == 2004| is.na(year))



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
                         
                         selectInput("colors", "Color Scheme", selected = "YlGn",
                                     rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
           )
           ),
           tabPanel("Data Explorer",
                    plotOutput("plot1")
                    
                    )
           
        )

# Define UI
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("SEAK Sablefish Catch per Unit Effort"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("slider1",
#                      "Choose a year:",
#                      min = 1997,
#                      max = 2018,
#                      value = 2000,
#                      sep = "")
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          # plotOutput("plot1"),
#          # plotOutput("plot2"), 
#          leafletOutput("map1")
#       )
#    )
# )

# Define server logic
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
  
  areacatch2 <- reactive(areacatch %>%
                           filter(year == input$slider1 | is.na(year)))

  #Colors
  colorpal <- reactive({
    colorNumeric(input$colors, areacatch2()$total_hooks_sablefish)
  })
 

  # bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, Inf)#what are the cutoffs for coloring these? anything between these ranges will be one color or another color
  # colors <-colorBin(palette = "YlGn", #whats the gradient I want
  #                   domain = areacatch2()$total_hooks_sablefish, #this is the thing I'm basing my color gradient off of
  #                   bins = bins)
  
  ###making chloropleth
  
  
   output$map1 <- renderLeaflet({
     ###Rendering leaflet for only the parts of the map that are not dynamic; ie, the shp files. 
     states %>%
       leaflet()%>%
       addTiles() %>%
       setView(-134.5, 57, 7.5) %>% 
       # addPolygons(data = simpleground,
       #             #fillColor = ~colors(areacatch2()$total_hooks_sablefish),
       #             weight = 2, #make darks darker and lights lighter
       #             opacity = 1,
       #             color = "white",
       #             dashArray = "3",
       #             fillOpacity = 0.7,
       #             popup = paste("STAT Area:", simpleground@data$STAT_AREA, "<br>",
       #                           "Sablefish Surveyed:", areacatch2()$total_hooks_sablefish
       #                           )) %>%
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
   #maybe just start out with leaflet proxy for color scheme...
   observe({
     pal <- colorpal()

     leafletProxy("map1") %>%
        clearShapes() %>%
        addPolygons(data = simpleground,
                    fillColor = ~pal(areacatch2()$total_hooks_sablefish),
                    weight = 2, #make darks darker and lights lighter
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    popup = paste("STAT Area:", simpleground@data$STAT_AREA, "<br>",
                                  "Sablefish Surveyed:", areacatch2()$total_hooks_sablefish
                    ))
                    

   })
   
   output$plot1 <- renderPlot({
     areacatch2() %>%
       filter(STAT_AREA == c(345731) | STAT_AREA == 345701 | STAT_AREA == 345631 | STAT_AREA == 345603) %>%
       ggplot(aes(x = STAT_AREA, y = total_hooks_sablefish)) +
       geom_bar(stat = "identity") +
       theme_classic(base_size = 15) 
   })
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

