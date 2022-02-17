# GBIF Occurrence mapping and Species distribution modeling
# Amanda Suzzi
# asuzzi@umass.edu
# 2021-11-29

library(readr)
library(tidyverse)
library(ggplot2)
library(maps)
library(raster)
library(rgbif)
library(data.table)
library(leaflet)
library(dismo)
library(rgdal)
library(rgeos)
#library(rJava)
#obs.data <- read_csv("GitHub/sifka/Sifka/data/sifka_all2021.1.csv", col_types = cols(...1 = col_skip(), Year = col_skip()))

myspecies <- "propithecus coquereli" # Change to your taxa here 


data <- occ_data(scientificName = myspecies,
                 hasCoordinate = TRUE,
                 hasGeospatialIssue = FALSE,
                 limit = 5000)  #number of occurancces to load


obs.data <- data[["data"]] %>%
  dplyr::select(longitude = decimalLongitude, latitude = decimalLatitude)%>% 
  drop_na() 

occmap <- leaflet::leaflet(obs.data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~longitude, ~latitude, 
                 radius = 1, weight = 2, opacity = 0.5, fill = TRUE, 
                 fillOpacity = 0.2)

occmap #map the occurrences. Do they make sense? You might have to crop to a specific area.

#set NW Madagascar as study area
mdg <- st_read("~/GitHub/sifka/Sifka/data/hull.shp")

#mdg <- getData('GADM', country='MDG', level=0)
obs.data <- SpatialPoints(obs.data, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
obs.data <- raster::crop(obs.data, mdg)
obs.data <- as.data.frame(obs.data)

occmap <- leaflet::leaflet(obs.data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~longitude, ~latitude, 
                   radius = 1, weight = 2, opacity = 0.5, fill = TRUE, 
                   fillOpacity = 0.2)

occmap #map the occurrences. Do they make sense now?

#### Species Distribution Model CODE

#To make species distribution modeling more streamlined, it is useful to have an idea of how widely our species is geographically distributed. We are going to find general latitudinal and longitudinal boundaries and store this information for later use:
max.lat <- ceiling(max(obs.data$latitude))
min.lat <- floor(min(obs.data$latitude))
max.lon <- ceiling(max(obs.data$longitude))
min.lon <- floor(min(obs.data$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))  
  

# downloading the bioclimatic variables from worldclim 
bioclim.data <- getData("worldclim", var="bio", res=10) #change res for finer grain
f.bioclim.data <- getData('CMIP5', var='bio', res=10, rcp=85, model='HE', year=70) #change res for finer grain
names(f.bioclim.data)=names(bioclim.data)

#restrict the biolimatic variable data to the geographic extent of our occurrence data
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)
f.bioclim.data <- crop(x = f.bioclim.data, y = geographic.extent)

# Build species distribution model
maxent.model <- maxent(bioclim.data, obs.data)

# plot showing importance of each variable
plot(maxent.model) #you would usually reduce your variables to the most important before you predict, but we're lazy

# response curves
response(maxent.model)

# predict to entire dataset
predict.presence <- predict(maxent.model,bioclim.data)

# make predictions with the future environment data
predict.2070 <- predict(maxent.model, f.bioclim.data)

map <- leaflet::leaflet(obs.data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~longitude, ~latitude, 
                   radius = 1, weight = 2, opacity = 0.5, fill = TRUE, 
                   fillOpacity = 0.2, group="Occurances")%>% 
  addRasterImage(predict.2070, group="Future Prediction")%>% 
  addRasterImage(predict.presence, group="Prediction")%>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("Occurances","Prediction", "Future Prediction"),
    options = layersControlOptions(collapsed = FALSE)
  )

map
