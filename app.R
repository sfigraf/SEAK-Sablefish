
#
library(tidyverse)
library(data.table)
library(geojsonio)
library(rgdal)
library(shiny)
library(leaflet)
library(scales)

data <- read_csv("llsrv_cpue_1985_2018.csv")

#Shp files
states <- geojson_json(states, geometry = "polygon", group = "group")
groundfish <- readOGR(".",layer = "Groundfish_Statistical_Areas_2001") #then read it out
groundfish <- spTransform(groundfish, CRS("+proj=longlat +datum=WGS84 +no_defs")) #put it into right form

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("SEAK Sablefish"),
   
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
         plotOutput("plot1"),
         plotOutput("plot2"), 
         leafletOutput("map1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  sablefishdata <- reactive(data %>%
                            na.omit() %>%
                             filter(year == input$slider1))
   output$plot1 <- renderPlot({
     sablefishdata() %>%
       ggplot(aes(x = julian_day, y = hooks_sablefish)) +
       geom_bar(stat = "identity", aes(fill = Stat)) +
       theme_classic(base_size = 15) + 
       scale_x_continuous(breaks = pretty_breaks()) +
       scale_color_manual(values = c("red",
                                     "green",
                                     "blue"))
   })
   
   output$plot2 <- renderPlot({
     data %>%
       na.omit() %>%
       group_by(year) %>%
       summarize(sablefish_surveyed = sum(hooks_sablefish)) %>%
       ggplot(aes(x = year, y = sablefish_surveyed)) +
       geom_bar(stat = "identity") +
       theme_classic(base_size = 15) 
   })
   
   output$map1 <- renderLeaflet({
     states %>%
       leaflet() %>%
       addTiles() %>%
       setView(-159, 61, 3.2) %>%
       addPolygons(data = groundfish,
                   popup = paste(groundfish@data$REGISTRATI,"<br>", groundfish@data$STAT_AREA),
                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                       bringToFront = TRUE))
   })
}

#if i can get it so the Stat area will be highlighted if you run your mouse over the 
# Run the application 
shinyApp(ui = ui, server = server)

