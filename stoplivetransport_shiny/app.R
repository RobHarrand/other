library(shiny)
library(leaflet)
library(leaflet.extras)
library(scales)
library(sp)

#setwd("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\stoplivetransport_shiny\\")

#stoplivetransport_pigs = readRDS(file='stoplivetransport_pigs.rds')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  leafletOutput("mymap", height = 500),
  
  column(6,
         tags$b(h3("Live pig transports across Europe in 2013")),
         tags$br(),
         tags$b(h4("INSTRUCTIONS")),
         h5("- Click a country marker to show total imports and exports"),
         h5("- Display export paths from the panel in the top-right"),
         h5("- Click a transport path for further details"),
         h5("- Zoom in/out using the mouse wheel or -/+ icons in the top left")
  ),
  
  column(4,
         tags$br(),
         h4("Citations / Further Info"),
         
         h6("Food and Agriculture Organization of the United Nations Detailed Trade Matrix"),
         h6(a("Detailed Trade Matrix Data link", target="_blank", href="http://www.fao.org/faostat/en/#data/TM")),
         
         h6("Stop Live Transport Campaign"),
         h6(a("Stop Live Transport website", target="_blank", href="https://stoplivetransport.org/")),
         
         img(src="stop_live_transport_logo_uk.png", height = 87, width = 257),
         
         h6("(Rob Harrand, September 2017)")
  )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$mymap <- renderLeaflet({
   
    fortifiedroutes_1 = read.csv('fortifiedroutes_1.csv')
    fortifiedroutes_2 = read.csv('fortifiedroutes_2.csv')
    fortifiedroutes_3 = read.csv('fortifiedroutes_3.csv')
    fortifiedroutes_4 = read.csv('fortifiedroutes_4.csv')
    ll = read.csv("geo2.csv")
    
    
    
    stoplivetransport_pigs <- leaflet(ll) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(ll$lng, 
                 ll$lat,
                 #icon=icon1,
                 popup = paste("Country: ", 
                               ll$Country,
                               "</br>",
                               "Total Exports: ", 
                               comma(ll$Export),
                               "</br>",
                               "Total Imports: ", 
                               comma(ll$Import),
                               sep = "")) %>%
      addLayersControl(overlayGroups = c("<1k pigs","1k-10k pigs","10k-100k pigs", "100k+ pigs") , options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("<1k pigs") %>%
      hideGroup("1k-10k pigs") %>%
      hideGroup("10k-100k pigs") %>%
      hideGroup("100k+ pigs")
    
    
    
    for(j in (1:239)) {
      
      stoplivetransport_pigs <- addPolylines(stoplivetransport_pigs, data = fortifiedroutes_1[fortifiedroutes_1$id == j,],
                                             lng = ~lng,
                                             lat = ~lat,
                                             group = '<1k pigs',
                                             popup = paste("From: ", 
                                                           fortifiedroutes_1$From[fortifiedroutes_1$id == j],
                                                           "</br>",
                                                           "To: ", 
                                                           fortifiedroutes_1$To[fortifiedroutes_1$id == j],
                                                           "</br>",
                                                           "Year: ", 
                                                           fortifiedroutes_1$Year[fortifiedroutes_1$id == j],
                                                           "</br>",
                                                           "Value: ", 
                                                           comma(fortifiedroutes_1$Quantity[fortifiedroutes_1$id == j]),
                                                           "</br>",
                                                           sep = ""),
                                             weight = log(fortifiedroutes_1$Quantity[fortifiedroutes_1$id == j])/5, 
                                             col = 'green', 
                                             opacity = 0.1)
      
    } 
    
    for(j in (1:239)) {
      
      stoplivetransport_pigs <- addPolylines(stoplivetransport_pigs, data = fortifiedroutes_2[fortifiedroutes_2$id == j,],
                                             lng = ~lng,
                                             lat = ~lat,
                                             group = '1k-10k pigs',
                                             popup = paste("From: ", 
                                                           fortifiedroutes_2$From[fortifiedroutes_2$id == j],
                                                           "</br>",
                                                           "To: ", 
                                                           fortifiedroutes_2$To[fortifiedroutes_2$id == j],
                                                           "</br>",
                                                           "Year: ", 
                                                           fortifiedroutes_2$Year[fortifiedroutes_2$id == j],
                                                           "</br>",
                                                           "Value: ", 
                                                           comma(fortifiedroutes_2$Quantity[fortifiedroutes_2$id == j]),
                                                           "</br>",
                                                           sep = ""),
                                             weight = log(fortifiedroutes_2$Quantity[fortifiedroutes_2$id == j])/5,
                                             col = 'blue', 
                                             opacity = 0.1)
      
    } 
    
    for(j in (1:239)) {
      
      stoplivetransport_pigs <- addPolylines(stoplivetransport_pigs, data = fortifiedroutes_3[fortifiedroutes_3$id == j,],
                                             lng = ~lng,
                                             lat = ~lat,
                                             group = '10k-100k pigs',
                                             popup = paste("From: ", 
                                                           fortifiedroutes_3$From[fortifiedroutes_3$id == j],
                                                           "</br>",
                                                           "To: ", 
                                                           fortifiedroutes_3$To[fortifiedroutes_3$id == j],
                                                           "</br>",
                                                           "Year: ", 
                                                           fortifiedroutes_3$Year[fortifiedroutes_3$id == j],
                                                           "</br>",
                                                           "Value: ", 
                                                           comma(fortifiedroutes_3$Quantity[fortifiedroutes_3$id == j]),
                                                           "</br>",
                                                           sep = ""),
                                             weight = log(fortifiedroutes_3$Quantity[fortifiedroutes_3$id == j])/5, 
                                             col = 'purple', 
                                             opacity = 0.1)
      
    } 
    
    for(j in (1:239)) {
      
      stoplivetransport_pigs <- addPolylines(stoplivetransport_pigs, data = fortifiedroutes_4[fortifiedroutes_4$id == j,],
                                             lng = ~lng,
                                             lat = ~lat,
                                             group = '100k+ pigs',
                                             popup = paste("From: ", 
                                                           fortifiedroutes_4$From[fortifiedroutes_4$id == j],
                                                           "</br>",
                                                           "To: ", 
                                                           fortifiedroutes_4$To[fortifiedroutes_4$id == j],
                                                           "</br>",
                                                           "Year: ", 
                                                           fortifiedroutes_4$Year[fortifiedroutes_4$id == j],
                                                           "</br>",
                                                           "Value: ", 
                                                           comma(fortifiedroutes_4$Quantity[fortifiedroutes_4$id == j]),
                                                           "</br>",
                                                           sep = ""),
                                             weight = log(fortifiedroutes_4$Quantity[fortifiedroutes_4$id == j])/5, 
                                             col = 'red', 
                                             opacity = 0.1)
      
    } 
    
    
    stoplivetransport_pigs
    
})

}

# Run the application 
shinyApp(ui = ui, server = server)

