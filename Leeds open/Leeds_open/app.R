library(shiny)
library(leaflet)
library(plyr)
library(ggplot2)
library(maptools)
library(shinyjs)
library(shinythemes)
library(rsconnect)

# input<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\msoa_2011_pop_cent.csv", sep=",", header=T)
# names<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\names.csv", sep=",", header=T)
# 
# cancer_mortality<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\U75CancerMortality.csv", sep=",", header=T)
# resp_mortality<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\U75RespiratoryMortality.csv", sep=",", header=T)
# circulatory_mortality<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\U75CirculatoryMortality.csv", sep=",", header=T)
# all_mortality<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\AAACM.csv", sep=",", header=T)
# 
# cancer_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\CancerAllAgesMILL.csv", sep=",", header=T)
# heartdisease_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\CHDAllAgesMILL.csv", sep=",", header=T)
# pulmonarydisease_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\COPDAllAgesMILL.csv", sep=",", header=T)
# diabetes_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\DiabetesAllAgesMILL.csv", sep=",", header=T)
# obesity_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\Obesity16plusMILL.csv", sep=",", header=T)
# smoking_rate<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\Smoking16plusMILL.csv", sep=",", header=T)
# 
# merged = merge(input, cancer_mortality, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, resp_mortality, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, circulatory_mortality, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, all_mortality, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, cancer_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, heartdisease_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, pulmonarydisease_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, diabetes_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, obesity_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# merged = merge(merged, smoking_rate, by.x = "MSOA11CD", by.y = "AreaCodeName")
# 
# merged = merge(merged, names, by.x = "MSOA11CD", by.y = "S.O.A.")
# 
# write.csv(merged, 'merged.csv')

#merged<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\Leeds_open\\merged.csv", sep=",", header=T)
merged<-read.table("merged.csv", sep=",", header=T)

 
#Index numbers
cancer_mortality = 8:61
resp_mortality = 62:116
circulatory_mortality = 117:171
all_mortality = 172:226

cancer_rate = 227:281
heartdisease_rate = 282:336
pulmonarydisease_rate = 337:391
diabetes_rate = 392:446
obesity_rate = 447:501
smoking_rate = 502:556

dates = c('10.2012',
          '01.2013','04.2013','07.2013','10.2013',
          '01.2014','04.2014','07.2014','10.2014',
          '01.2015','04.2015','07.2015','10.2015',
          '01.2016','04.2016','07.2016','10.2016')
daterange = c('2006.2010','2007.2011','2008.2012','2009.2013','2010.2014','2011.2015')
opts_m = c('NONE', 'cancer_mortality', 'resp_mortality', 'circulatory_mortality', 'all_mortality')
opts_r = c('NONE', 'cancer_rate', 'heartdisease_rate', 'pulmonarydisease_rate', 'diabetes_rate', 'obesity_rate', 'smoking_rate')
type = c('Persons', 'Male', 'Female')


ui <- fluidPage(
  leafletOutput("mymap"),
  
  h5("All values are 'Directly Age Standardised Mortality Rates' (DASR) per 100,000"),
  h5("Each point represents the relevant (scaled) DASR value, with corresponding smaller and larger circles representing the lower and upper confidence intervals, respectively"),
  
  column(4,
  h2("Mortality Rates"),
  selectInput(inputId = "m_mortality", label = "Select Mortality Rate", 
              choices = opts_m),
  selectInput(inputId = "m_daterange", label = "Select Date Range", 
              choices = daterange),
  selectInput(inputId = "m_type", label = "Select Type",
              choices = type),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br()
),

column(4,
       h2("Disease Rates"),
       selectInput(inputId = "r_disease", label = "Select Disease Rate", 
                   choices = opts_r),
       selectInput(inputId = "r_date", label = "Select Month and Year", 
                   choices = dates),
       actionButton('display', 'Display Points'),
       p()
),

#url1 <- a("Mortality data", href="https://www.google.com/")
#url2 <- a("Disease rates data", href="https://www.google.com/")

column(4,
       
       h4("Citations"),
       
       h6("Mortality data: Public Health data - GP recorded conditions Leeds, (c) Leeds City Council, 2017. This information is licensed under the terms of the Open Government Licence."),
       h6(a("Mortality data link", target="_blank", href="https://datamillnorth.org/dataset/public-health-data---mortality-rates")),
       
       h6("Disease rates data: Public Health data - Mortality rates Leeds, (c) Leeds City Council, 2017 .  This information is licensed under the terms of the Open Government Licence."),
       h6(a("Disease rates data link", target="_blank", href="https://datamillnorth.org/dataset/public-health-data---gp-recorded")),
       
       h6("Population-weighted centroids: The National Archives, 2011 .  Contains National Statistics data (c) Crown copyright and database right [2011]."),
       h6(a("Population-weighted centroids data link", target="_blank", href="http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/centroids/index.html")),
       
       h6("Middle Layer SOA names: Super Output Area Profiler."),
       h6(a("Middle Layer SOA names data link", target="_blank", href="http://www.bmb.leeds.ac.uk/illingworth/SOAP/default.html")),
       
       h6("(Rob Harrand, September 2017)")
)
)



server <- function(input, output, session) {
  
observeEvent(input$display, {

  #Mortalities
  
  if (input$m_mortality == 'NONE') {index_m = 0}
  if (input$m_mortality == 'cancer_mortality') {index_m = cancer_mortality}
  if (input$m_mortality == 'resp_mortality') {index_m = resp_mortality}
  if (input$m_mortality == 'circulatory_mortality') {index_m = circulatory_mortality}
  if (input$m_mortality == 'all_mortality') {index_m = all_mortality}
  
  #Find DSR
  x1=grep(input$m_daterange, colnames(merged))
  y1=grep(input$m_type, colnames(merged))
  z1=grep('dsr', colnames(merged))
  col_m_main = intersect(intersect(intersect(x1,y1),z1), index_m)
  
  #Find LOWER
  x1=grep(input$m_daterange, colnames(merged))
  y1=grep(input$m_type, colnames(merged))
  z1=grep('lower', colnames(merged))
  col_m_lower = intersect(intersect(intersect(x1,y1),z1), index_m)
  
  #Find UPPER
  x1=grep(input$m_daterange, colnames(merged))
  y1=grep(input$m_type, colnames(merged))
  z1=grep('upper', colnames(merged))
  col_m_upper = intersect(intersect(intersect(x1,y1),z1), index_m)
  

 
  #Rates
  
  if (input$r_disease == 'NONE') {index_r = 0}
  if (input$r_disease == 'cancer_rate') {index_r = cancer_rate}
  if (input$r_disease == 'heartdisease_rate') {index_r = heartdisease_rate}
  if (input$r_disease == 'pulmonarydisease_rate') {index_r = pulmonarydisease_rate}
  if (input$r_disease == 'diabetes_rate') {index_r = diabetes_rate}
  if (input$r_disease == 'obesity_rate') {index_r = obesity_rate}
  if (input$r_disease == 'smoking_rate') {index_r = smoking_rate}
  
  #Find DSR
  x2=grep(input$r_date, colnames(merged))
  z2=grep('DASR', colnames(merged))
  col_r_main = intersect(intersect(x2,z2), index_r)
  
  #Find LOWER
  x2=grep(input$r_date, colnames(merged))
  z2=grep('lower', colnames(merged))
  col_r_lower = intersect(intersect(x2,z2), index_r)

  #Find UPPER
  x2=grep(input$r_date, colnames(merged))
  z2=grep('upper', colnames(merged))
  col_r_upper = intersect(intersect(x2,z2), index_r)
  
  if(length(col_m_main) == 0) {m_max_val = 1} else {m_max_val = max(merged[,col_m_main])}
  if(length(col_r_main) == 0) {r_max_val = 1} else {r_max_val = max(merged[,col_r_main])}
  

  #Map
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    
    leaflet(merged) %>%
      addTiles() %>%
  addCircleMarkers(      #Mortality Lower
      radius = (merged[,col_m_lower]/m_max_val)*15,
      color = 'blue',
      stroke = FALSE, 
      fillOpacity = 0.2) %>%
  addCircleMarkers(      #Mortality Upper
    radius = (merged[,col_m_upper]/m_max_val)*15,
    color = 'blue',
    stroke = FALSE, 
    fillOpacity = 0.2) %>%
  addCircleMarkers(      #Mortality Main
    radius = (merged[,col_m_main]/m_max_val)*15,
    color = 'red',
    stroke = FALSE, 
    fillOpacity = 0.3,
    popup = paste("Area: ", merged$Name, 
                  "</br>", 
                  "Point type: ", input$m_mortality, 
                  "</br>", 
                  "Date Range: ", input$m_daterange, 
                  "</br>", 
                  "Type: ", input$m_type, 
                  "</br>", 
                  "Value: ", merged[,col_m_main], 
                  sep = "")) %>%
  addCircleMarkers(      #Rates Lower
    radius = (merged[,col_r_lower]/r_max_val)*15,
    color = 'blue',
    stroke = FALSE, 
    fillOpacity = 0.2) %>%
      addCircleMarkers(      #Rates Upper
    radius = (merged[,col_r_upper]/r_max_val)*15,
    color = 'blue',
    stroke = FALSE, 
    fillOpacity = 0.2) %>% 
      addCircleMarkers(      #Rates Main
    radius = (merged[,col_r_main]/r_max_val)*15,
    color = 'green',
    stroke = FALSE, 
    fillOpacity = 0.3,
    popup = paste("Area: ", merged$Name, 
                  "</br>", 
                  "Point type: ", input$r_disease, 
                  "</br>", 
                  "Date: ", input$r_date, 
                  "</br>", 
                  "Value: ", merged[,col_r_main], 
                  sep = ""))
})
})
}

shinyApp(ui, server)


