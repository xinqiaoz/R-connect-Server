library(tidyverse)
library(magrittr)
library(readxl)
library(ggmap)
library(shiny)
library(leaflet)
library(maps)
library(htmlwidgets) # To save the map as a web page.
# data = read.csv("tmphbl23vi6.csv")
# 

# 
# data %<>% filter(YEAR == 2019)
# write.csv(data,"data.csv")
data = read.csv("tmphbl23vi6.csv") %>% select(INCIDENT_NUMBER,OFFENSE_CODE,YEAR,MONTH,Lat,Long)
code <- read_excel("rmsoffensecodes.xlsx")
police = read.csv("Boston_Police_Station.csv") %>% rename("Long"="LONG","Lat" = "LAT") %>% select(Long,Lat,NAME)

data = left_join(x = data,y = code,by = c("OFFENSE_CODE"="CODE"))
data = na.omit(data)





############################################################################################################################################
library(shiny)

ui = fluidPage(
  title = "Crime and Police Station Distribution",
  verbatimTextOutput("Text"),
  tabsetPanel(
    tabPanel("Map",leafletOutput(outputId = "Plot",width = 1600, height = 800)),
    tabPanel("Data",tableOutput("table"))
  ),
  
  
  hr(),
  
  fluidRow(
    column(3,
           h4("Select how many crimes to show"),
           sliderInput("nu","Number of Crimes to Show",min = 1,max = 100,value = 30,
                       step = 1)),
    column(4,offset = 1,
           selectInput("cod", "Select an Offense Code", unique(data$OFFENSE_CODE) ),
           selectInput("ye", "Select Year", unique(data$YEAR) )
           ),
    column(4,
           selectInput("mo", "Select Month", unique(data$MONTH) ))
  )
)


server <- function(input, output, session) {
  
  output$Text = renderPrint({
    "Blue marks represents Crime, Red marks represents Police Stations"
  })
  
  output$Plot = renderLeaflet({
    
    sub_da = data %>% filter(OFFENSE_CODE == input$cod & YEAR == input$ye & MONTH == input$mo)
    sites = head(sub_da,input$nu)
    # State boundaries from the maps package. The fill option must be TRUE.
    bounds <- map(database = "state", c('Massachusetts'), fill=TRUE, plot=FALSE)
    # A custom icon.
    icons <- awesomeIcons(
      icon = 'disc',
      iconColor = 'black',
      library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
      markerColor = 'blue',
      squareMarker = F
    )
    
    icons_po <- awesomeIcons(
      icon = 'home',
      iconColor = 'fa',
      library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
      markerColor = 'red',
      squareMarker = T,
      spin = T
    )
    
    # Create the Leaflet map widget and add some map layers.
    # We use the pipe operator %>% to streamline the adding of
    leaflet(data = sites) %>%
      setView(mean(sites$Long), mean(sites$Lat), zoom = 12) %>% 
    # layers to the leaflet object. The pipe operator comes from 
    # the magrittr package via the dplyr package.
      addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
      addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
      # Marker data are from the sites data frame. We need the ~ symbols
      # to indicate the columns of the data frame.
      addAwesomeMarkers(lng = ~police$Long, lat = ~police$Lat, label = ~police$NAME, group = "Police Station", icon=icons_po) %>% 
      addAwesomeMarkers(lng = ~Long, lat = ~Lat, label = ~NAME, group = "OFFENSE", icon=icons) %>%
      addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Map", "Satellite", "Relief"),
        overlayGroups = c("OFFENSE", "States","Police Station"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$table = renderTable({
    sub_da = data %>% filter(OFFENSE_CODE == input$cod & YEAR == input$ye & MONTH == input$mo)
    sites = head(sub_da,input$nu)
    sites
  })

}

shinyApp(ui = ui, server = server)

