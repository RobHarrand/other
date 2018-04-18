library(rgdal)
library(lubridate)
library(leaflet.extras)
library(leaflet)
library(shiny)

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
 #leeds <- readOGR("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Cycle_routes\\PLAN_CYCLE_ROUTES.shp",
#                   GDAL1_integer64_policy = TRUE)
 
load('leeds.Rdata')
#load('C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Cycle_routes\\leeds_cycle\\leeds.Rdata')
#save(leeds, file='leeds.Rdata')


#leeds <- readOGR("PLAN_CYCLE_ROUTES.shp", GDAL1_integer64_policy = TRUE)
                 

PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
DAT <- sp::spTransform(leeds,PRO)


#Cycle casualties data

#cycle_casualties = read.csv("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Cycle_routes\\leeds_cycle\\cycle_casualties.csv")
cycle_casualties = read.csv("cycle_casualties.csv")
cycle_bays = read.csv("cycle_bays.csv")

#cycle_casualties$Accident.Date = as.character(cycle_casualties$Accident.Date)
cycle_casualties$Accident.Date = as.POSIXct(cycle_casualties$Accident.Date, format = '%d/%m/%Y')
cycle_casualties$Day = day(cycle_casualties$Accident.Date)
cycle_casualties$Month = month(cycle_casualties$Accident.Date)
cycle_casualties$Year = year(cycle_casualties$Accident.Date)

#cycle_casualties$X1st.Road.Class = as.character(cycle_casualties$X1st.Road.Class)
cycle_casualties$X1st.Road.Class[cycle_casualties$X1st.Road.Class == 3] = 'A'
cycle_casualties$X1st.Road.Class[cycle_casualties$X1st.Road.Class == 4] = 'B'
cycle_casualties$X1st.Road.Class[cycle_casualties$X1st.Road.Class == 5] = 'C'
cycle_casualties$X1st.Road.Class[cycle_casualties$X1st.Road.Class == 6] = 'Unclassified'

#cycle_casualties$Road.Surface = as.character(cycle_casualties$Road.Surface)
cycle_casualties$Road.Surface[cycle_casualties$Road.Surface == 1] = 'Dry'
cycle_casualties$Road.Surface[cycle_casualties$Road.Surface == 2] = 'Wet/damp'
cycle_casualties$Road.Surface[cycle_casualties$Road.Surface == 3] = 'Snow'
cycle_casualties$Road.Surface[cycle_casualties$Road.Surface == 4] = 'Frost/Ice'

cycle_casualties$Lighting.Conditions[cycle_casualties$Lighting.Conditions == 1] = 'Daylight: Street lights present'
cycle_casualties$Lighting.Conditions[cycle_casualties$Lighting.Conditions == 4] = 'Darkness: Street lights present and lit'
cycle_casualties$Lighting.Conditions[cycle_casualties$Lighting.Conditions == 5] = 'Darkness: Street lights present but unlit'
cycle_casualties$Lighting.Conditions[cycle_casualties$Lighting.Conditions == 6] = 'Darkness: No street lighting'
cycle_casualties$Lighting.Conditions[cycle_casualties$Lighting.Conditions == 7] = 'Darkness: Street lighting unknown'

cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 1] = 'Fine without high winds'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 2] = 'Raining without high winds'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 3] = 'Snowing without high winds'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 4] = 'Fine with high winds'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 5] = 'Raining with high winds'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 7] = 'Fog or mist'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 8] = 'Other'
cycle_casualties$Weather.Conditions[cycle_casualties$Weather.Conditions == 9] = 'Unknown'

cycle_casualties$Casualty.Class[cycle_casualties$Casualty.Class == 1] = 'Driver or rider'
cycle_casualties$Casualty.Class[cycle_casualties$Casualty.Class == 2] = 'Vehicle or pillion passenger'
cycle_casualties$Casualty.Class[cycle_casualties$Casualty.Class == 3] = 'Pedestrian'

cycle_casualties$Casualty.Severity[cycle_casualties$Casualty.Severity == 1] = 'Fatal'
cycle_casualties$Casualty.Severity[cycle_casualties$Casualty.Severity == 2] = 'Serious'
cycle_casualties$Casualty.Severity[cycle_casualties$Casualty.Severity == 3] = 'Slight'

cycle_casualties$Sex.of.Casualty[cycle_casualties$Sex.of.Casualty == 1] = 'Male'
cycle_casualties$Sex.of.Casualty[cycle_casualties$Sex.of.Casualty == 2] = 'Female'

cycle_casualties$Type.of.Vehicle[cycle_casualties$Type.of.Vehicle == 1] = 'Pedal cycle'

cycle_casualties_slight = cycle_casualties[cycle_casualties$Casualty.Severity == 'Slight',]
cycle_casualties_serious = cycle_casualties[cycle_casualties$Casualty.Severity == 'Serious',]
cycle_casualties_fatal = cycle_casualties[cycle_casualties$Casualty.Severity == 'Fatal',]





# Define UI for application that draws a histogram
ui <- fluidPage(
   
  leafletOutput("mymap", height = 500),
  
column(6,
  h3("Cycle Routes, Cycle Bays & Cycle Accidents in the Leeds Area"),
  h5("The blue lines show the cycle network"),
  h5("The overlaid heatmap shows regions of increased accident rates"),
  h5("For accidents, click one or more 'seriousness' levels from the top-right panel to display points"),
  h5("For cycle bays, click to turn these on or off via the same panel"),
  h5("Click a point to show bay details / accident details")
),

column(4,
  tags$br(),
  
  h4("Citations"),
  
  h6("Cycle Route Data: Cycle routes in Leeds, (c) Leeds City Council, 2017. This information is licensed under the terms of the Open Government Licence."),
  h6(a("Cycle Route Data link", target="_blank", href="https://datamillnorth.org/dataset/cycle-routes-in-leeds")),
  
  h6("Cycling Casualties Data: Cycling casualties in Leeds, (c) Student Data Labs, 2015 .  This work is licensed under a Creative Commons Attribution License."),
  h6(a("Cycling Casualties Data link", target="_blank", href="https://datamillnorth.org/dataset/cycling-casualties-in-leeds")),
  
  h6("Cycle Bays Data: Leeds city centre bike bays, (c) Leeds City Council, 2017 .  This information is licensed under the terms of the Open Government Licence."),
  h6(a("Cycle Bays Data link", target="_blank", href="https://datamillnorth.org/dataset/leeds-city-centre-bike-bays")),
  
  h6("(Rob Harrand, September 2017)")
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # #Map
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  
  icon1 <- awesomeIcons(
    icon = 'ion-android-alert',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'green'
  )
  
  icon2 <- awesomeIcons(
    icon = 'ion-android-alert',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'yellow'
  )
  
  icon3 <- awesomeIcons(
    icon = 'ion-android-alert',
    iconColor = 'red',
    library = 'ion',
    markerColor = 'black'
  )
  
  icon4 <- awesomeIcons(
    icon = 'ion-android-bicycle',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'blue'
  )
  
  output$mymap <- renderLeaflet({
    
    leaflet(DAT) %>%
      #addTiles() %>%  # Add default OpenStreetMap map tiles
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addMarkers(lat=cycle_bays$Lat, lng=cycle_bays$Long) %>%
      addAwesomeMarkers(lat=cycle_casualties_slight$Latitude, 
                 lng=cycle_casualties_slight$Longitude,
                 group = 'Slight',
                 icon=icon1,
                 popup = paste("Accident date: ", cycle_casualties_slight$Accident.Date, 
                               "</br>", 
                               "Time: ", cycle_casualties_slight$Time..24hr., 
                               "</br>",
                               "Number of vehicles: ", cycle_casualties_slight$Number.of.Vehicles, 
                               "</br>", 
                               "Number of casualities: ", cycle_casualties_slight$Number.of.Casualties, 
                               "</br>", 
                               "Road class: ", cycle_casualties_slight$Road.Surface, 
                               "</br>", 
                               "Lighting conditions: ", cycle_casualties_slight$Lighting.Conditions, 
                               "</br>", 
                               "Weather conditions: ", cycle_casualties_slight$Weather.Conditions, 
                               "</br>", 
                               "Casualty class: ", cycle_casualties_slight$Casualty.Class, 
                               "</br>", 
                               "Casualty severity: ", cycle_casualties_slight$Casualty.Severity, 
                               "</br>", 
                               "Sex of casualty: ", cycle_casualties_slight$Sex.of.Casualty, 
                               "</br>", 
                               "Age of casualty: ", cycle_casualties_slight$Age.of.Casualty,
                               sep = "")) %>%
      addAwesomeMarkers(lat=cycle_casualties_serious$Latitude, 
                 lng=cycle_casualties_serious$Longitude,
                 group = 'Serious',
                 icon=icon2,
                 popup = paste("Accident date: ", cycle_casualties_serious$Accident.Date, 
                               "</br>", 
                               "Time: ", cycle_casualties_serious$Time..24hr., 
                               "</br>", 
                               "Number of vehicles: ", cycle_casualties_serious$Number.of.Vehicles, 
                               "</br>", 
                               "Number of casualities: ", cycle_casualties_serious$Number.of.Casualties, 
                               "</br>", 
                               "Road class: ", cycle_casualties_serious$Road.Surface, 
                               "</br>", 
                               "Lighting conditions: ", cycle_casualties_serious$Lighting.Conditions, 
                               "</br>", 
                               "Weather conditions: ", cycle_casualties_serious$Weather.Conditions, 
                               "</br>", 
                               "Casualty class: ", cycle_casualties_serious$Casualty.Class, 
                               "</br>", 
                               "Casualty severity: ", cycle_casualties_serious$Casualty.Severity, 
                               "</br>", 
                               "Sex of casualty: ", cycle_casualties_serious$Sex.of.Casualty, 
                               "</br>", 
                               "Age of casualty: ", cycle_casualties_serious$Age.of.Casualty,
                               sep = "")) %>%
      addAwesomeMarkers(lat=cycle_casualties_fatal$Latitude, 
                 lng=cycle_casualties_fatal$Longitude,
                 group = 'Fatal',
                 icon=icon3,
                 popup = paste("Accident date: ", cycle_casualties_fatal$Accident.Date, 
                               "</br>", 
                               "Time: ", cycle_casualties_fatal$Time..24hr., 
                               "</br>", 
                               "Number of vehicles: ", cycle_casualties_fatal$Number.of.Vehicles, 
                               "</br>", 
                               "Number of casualities: ", cycle_casualties_fatal$Number.of.Casualties, 
                               "</br>", 
                               "Road class: ", cycle_casualties_fatal$Road.Surface, 
                               "</br>", 
                               "Lighting conditions: ", cycle_casualties_fatal$Lighting.Conditions, 
                               "</br>", 
                               "Weather conditions: ", cycle_casualties_fatal$Weather.Conditions, 
                               "</br>", 
                               "Casualty class: ", cycle_casualties_fatal$Casualty.Class, 
                               "</br>", 
                               "Casualty severity: ", cycle_casualties_fatal$Casualty.Severity, 
                               "</br>", 
                               "Sex of casualty: ", cycle_casualties_fatal$Sex.of.Casualty, 
                               "</br>", 
                               "Age of casualty: ", cycle_casualties_fatal$Age.of.Casualty,
                               sep = "")) %>%
      addAwesomeMarkers(lat=cycle_bays$Lat, 
                        lng=cycle_bays$Long,
                        group = 'Cycle Bays',
                        icon=icon4,
                        popup = paste("Street: ", cycle_bays$Street,
                                      "</br>",
                                      "Type: ", cycle_bays$Type, 
                                      "</br>", 
                                      "Colour: ", cycle_bays$Colour,
                                      sep = "")) %>%
      addPolylines(weight = 4, opacity = 1) %>%
      addWebGLHeatmap(lat=cycle_casualties$Latitude, 
                      lng=cycle_casualties$Longitude, 
                      size = 2500,
                      opacity = 0.35) %>%
    addLayersControl(overlayGroups = c("Slight","Serious","Fatal", "Cycle Bays") , options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Slight") %>%
      hideGroup("Serious") %>%
      hideGroup("Fatal")  %>%
      hideGroup("Cycle Bays")
    
  })

}
   

# Run the application 
shinyApp(ui = ui, server = server)
