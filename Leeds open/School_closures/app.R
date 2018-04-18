library(rgdal)
library(lubridate)
library(leaflet.extras)
library(leaflet)
library(shiny)
library(dplyr)

schools = read.csv("calderdale_school_closures.csv", stringsAsFactors = F)
#met = read.csv("brad_temps.csv", stringsAsFactors = F)

#schools$Latitude = schools$Latitude + rnorm(length(schools$Latitude), mean=0.0001, sd=0.0001)
#schools$Longitude = schools$Longitude + rnorm(length(schools$Longitude), mean=0.0001, sd=0.0001)


#range(schools$EFF_DATE_FROM)
#met = met[met$Year %in% 2009:2018,]

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
#leeds <- readOGR("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Cycle_routes\\PLAN_CYCLE_ROUTES.shp",
#                   GDAL1_integer64_policy = TRUE)

#load('leeds.Rdata')
#load('C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Cycle_routes\\leeds_cycle\\leeds.Rdata')
#save(leeds, file='leeds.Rdata')


#leeds <- readOGR("PLAN_CYCLE_ROUTES.shp", GDAL1_integer64_policy = TRUE)

schools$date_month = substring(schools$EFF_DATE_FROM, 4, 9)
schools$EFF_DATE_FROM = as.Date(schools$EFF_DATE_FROM, format = '%d-%b-%y')
schools$EFF_DATE_TO = as.Date(schools$EFF_DATE_TO, format = '%d-%b-%y')

snow = schools[grepl("snow", schools$REASON, ignore.case = T),]
strike = schools[grepl("strike", schools$REASON, ignore.case = T),]

# met$Month = sapply(as.integer(met$Month), function(n) sprintf("%02d", n))
# met$Month = month.abb[met$Month]
# met$Year = as.character(met$Year)
# met$Year = substring(met$Year, 3, 4)
# met$date_month = paste(met$Month, met$Year, sep = "-")
# 
# merged = merge(schools, met, by = 'date_month', all.x = T)
# 
snow_by_est = as.data.frame(table(snow$EST_NAME))
strike_by_est = as.data.frame(table(strike$EST_NAME))


# merged = merged[order(merged$EST_NAME, merged$EFF_DATE_FROM, decreasing = T),]
# merged = merged[!duplicated(merged$EST_NAME),]
# merged = merged[order(merged$EST_NAME),]
# merged$freq = by_est$Freq

snow = snow[order(snow$EST_NAME, snow$EFF_DATE_FROM, decreasing = T),]
snow = snow[!duplicated(snow$EST_NAME),]
snow = snow[order(snow$EST_NAME),]
snow$freq = snow_by_est$Freq

strike = strike[order(strike$EST_NAME, strike$EFF_DATE_FROM, decreasing = T),]
strike = strike[!duplicated(strike$EST_NAME),]
strike = strike[order(strike$EST_NAME),]
strike$freq = strike_by_est$Freq



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  leafletOutput("mymap", height = 500),
  
  column(6,
         h3("Emergency School Closures in the Calderdale Area"),
         h5("- The overlaid heatmap shows regions of school locations"),
         h5("- School positions are approximate (based upon postcode + small random shift to prevent points overlaying)"),
         h5("- For snow-related closures, click 'Snow' from the top-right panel to display points"),
         h5("- For strike-related closures, click 'Strike' from the top-right panel to display points"),
         h5("- Click a point to display details"),
         h5("- Use the mouse-wheel to zoom in/out")
  ),
  
  column(4,
         tags$br(),
         
         h4("Citations"),
         
         h6("Emergency School Closures Data: Calderdale Council, 2018. This information is licensed under the terms of the UK Open Government Licence (OGL v3)"),
         h6(a("Emergency School Closures Data link", target="_blank", href="https://dataworks.calderdale.gov.uk/dataset/emergency-school-closures")),
         
         h6(a("Postcode batch converter tool link", target="_blank", href='https://gridreferencefinder.com/postcodeBatchConverter/')),
         h6("(Rob Harrand, March 2018)")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # #Map
  # points <- eventReactive(input$recalc, {
  #   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  # }, ignoreNULL = FALSE)
  
  
  icon1 <- awesomeIcons(
    icon = 'ion-ios-snowy',
    iconColor = 'white',
    library = 'ion',
    markerColor = 'gray'
  )
  
  icon2 <- awesomeIcons(
    icon = 'ion-alert',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'red'
  )
  
  

#Thanks to https://gridreferencefinder.com/postcodeBatchConverter/
  
  output$mymap <- renderLeaflet({
    
    leaflet(snow) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addMarkers(lat=cycle_bays$Lat, lng=cycle_bays$Long) %>%
      addAwesomeMarkers(lat=snow$Latitude,
                        lng=snow$Longitude,
                        group = 'Snow',
                        icon=icon1,
                        popup = paste("School: ", snow$EST_NAME, 
                                      "</br>",
                                      "Number of Snow-related Closures: ", snow$freq, 
                                      "</br>",
                                      "Most Recent Snow-related Closure: ", snow$EFF_DATE_FROM, 
                                      sep = "")) %>%
      addAwesomeMarkers(lat=strike$Latitude, 
                        lng=strike$Longitude,
                        group = 'Strike',
                        icon=icon2,
                        popup = paste("School: ", strike$EST_NAME, 
                                      "</br>",
                                      "Number of Strike-related Closures: ", strike$freq, 
                                      "</br>",
                                      "Most Recent Strike-related Closure: ", strike$EFF_DATE_FROM, 
                                      sep = "")) %>%
      #addPolylines(weight = 4, opacity = 1) %>%
      addWebGLHeatmap(lat=schools$Latitude, 
                      lng=schools$Longitude, 
                      size = 2500,
                      opacity = 0.35) %>%
      addLayersControl(overlayGroups = c("Snow","Strike") , options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Snow") %>%
      hideGroup("Strike")
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)


