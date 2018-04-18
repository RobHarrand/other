# library(proj4)
# proj4string <- "+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
# 
# ?proj4string
# 
# # Source data
# xy <- data.frame(x=532464.0755, y=181219.6875)
# 
# # Transformed data
# pj <- project(xy, proj4string, inverse=TRUE)
# latlon <- data.frame(lat=pj$y, lon=pj$x)
# print(latlon)


library(plyr)
library(ggplot2)
library(maptools)

library(leaflet)

input<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\msoa_2011_pop_cent.csv", sep=",", header=T)
obesity<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\obesity.csv", sep=",", header=T)
names<-read.table("C:\\Users\\rob.harrand\\Desktop\\WORK\\Data hobbies\\Leeds open\\names.csv", sep=",", header=T)

merged = merge(input, obesity, by.x = "MSOA11CD", by.y = "AreaCodeName")
merged = merge(merged, names, by.x = "MSOA11CD", by.y = "S.O.A.")


m <- leaflet(merged) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(
    radius = merged$X01.10.2016_DASR/1000,
    color = 'red',
    stroke = FALSE, 
    fillOpacity = 0.3,
    popup = paste("Area: ", merged$Name, "</br>", "Diabetes DASR (2016): ", merged$X01.10.2016_DASR, sep = ""))
m  # Print the map