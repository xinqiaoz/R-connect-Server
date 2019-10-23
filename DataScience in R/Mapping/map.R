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
police = read.csv("Boston_Police_Stations.csv") %>% rename("Long"="ï..X","Lat" = "Y") %>% select(Long,Lat,NAME)

data = left_join(x = data,y = code,by = c("OFFENSE_CODE"="CODE"))
data = na.omit(data)



# cod = 3114
# ye = 2019
# mo = 9
# 
# sub_da = data %>% filter(OFFENSE_CODE == cod & YEAR == te & MONTH == mo)
# 
# #####################################################################################################################################
# 
# library(leaflet)
# library(maps)
# library(htmlwidgets) # To save the map as a web page.
# 
# # The data to map.
# sites = sub_da
# # State boundaries from the maps package. The fill option must be TRUE.
# bounds <- map(database = "state", c('Massachusetts'), fill=TRUE, plot=FALSE)
# # A custom icon.
# icons <- awesomeIcons(
#   icon = 'disc',
#   iconColor = 'black',
#   library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
#   markerColor = 'blue',
#   squareMarker = F
# )
# 
# icons_po <- awesomeIcons(
#   icon = 'disc',
#   iconColor = 'white',
#   library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
#   markerColor = 'red',
#   squareMarker = F
# )
# 
# # Create the Leaflet map widget and add some map layers.
# # We use the pipe operator %>% to streamline the adding of
# # layers to the leaflet object. The pipe operator comes from 
# # the magrittr package via the dplyr package.
# map <- leaflet(data = sites) %>%
#   # setView(-72.14600, 43.82977, zoom = 8) %>% 
#   addProviderTiles("CartoDB.Positron", group = "Map") %>%
#   addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
#   addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
#   # Marker data are from the sites data frame. We need the ~ symbols
#   # to indicate the columns of the data frame.
#   addAwesomeMarkers(lng = ~police$Long, lat = ~police$Lat, label = ~police$NAME, group = "Police Station", icon=icons_po) %>% 
#   addAwesomeMarkers(lng = ~Long, lat = ~Lat, label = ~OFFENSE_CODE, group = "OFFENSE", icon=icons) %>%
#   addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
#   addScaleBar(position = "bottomleft") %>%
#   addLayersControl(
#     baseGroups = c("Map", "Satellite", "Relief"),
#     overlayGroups = c("OFFENSE", "States","Police Station"),
#     options = layersControlOptions(collapsed = T)
#   )
# invisible(print(map))
# 
# ############################################################################################################################################
# 
# f = function(cod = 1501,ye = 2018, mo = 1){
#   
#   sub_da = data %>% filter(OFFENSE_CODE == cod & YEAR == ye & MONTH == mo)
#   sites = head(sub_da,30)
#   # State boundaries from the maps package. The fill option must be TRUE.
#   bounds <- map(database = "state", c('Massachusetts'), fill=TRUE, plot=FALSE)
#   # A custom icon.
#   icons <- awesomeIcons(
#     icon = 'disc',
#     iconColor = 'black',
#     library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
#     markerColor = 'blue',
#     squareMarker = F
#   )
#   
#   icons_po <- awesomeIcons(
#     icon = 'home',
#     iconColor = 'fa',
#     library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
#     markerColor = 'red',
#     squareMarker = F,
#     spin = T
#   )
#   
#   # Create the Leaflet map widget and add some map layers.
#   # We use the pipe operator %>% to streamline the adding of
#   # layers to the leaflet object. The pipe operator comes from 
#   # the magrittr package via the dplyr package.
#   map <- leaflet(data = sites) %>%
#     # setView(-72.14600, 43.82977, zoom = 8) %>% 
#     addProviderTiles("CartoDB.Positron", group = "Map") %>%
#     addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
#     addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
#     # Marker data are from the sites data frame. We need the ~ symbols
#     # to indicate the columns of the data frame.
#     addAwesomeMarkers(lng = ~police$Long, lat = ~police$Lat, label = ~police$NAME, group = "Police Station", icon=icons_po) %>% 
#     addAwesomeMarkers(lng = ~Long, lat = ~Lat, label = ~NAME, group = "OFFENSE", icon=icons) %>%
#     addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
#     addScaleBar(position = "bottomleft") %>%
#     addLayersControl(
#       baseGroups = c("Map", "Satellite", "Relief"),
#       overlayGroups = c("OFFENSE", "States","Police Station"),
#       options = layersControlOptions(collapsed = T)
#     )
#   invisible(print(map))
#   
# }

############################################################################################################################################
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("World Urban Population Percentage"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # selectInput("year1", "Select a base year", 
      #             choices = c( 1990=`1990`, "1991"=`1991`, "1992"=`1992`)),
      # selectInput("year2", "Select a year to compare", 
      #             choices = c( "1990"=`1990`, "1991"=`1991`, "1992"=`1992`))
      
      selectInput("cod", "Select an Offense Code", unique(data$OFFENSE_CODE) ),
      selectInput("ye", "Select Year", unique(data$YEAR) ),
      selectInput("mo", "Select Month", unique(data$MONTH) ),
      numericInput("nu", "Select how many crimes to show",value = 30)
    ),
    
    mainPanel(
      leafletOutput(outputId = "Plot",width = 1200, height = 800)
    )
  )
)


server <- function(input, output, session) {
  
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
      setView(-71.098849, 42.350434, zoom = 12) %>% 
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

}

shinyApp(ui = ui, server = server)

