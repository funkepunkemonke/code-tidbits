# Amanda Suzzi
# asuzzi@umass.edu

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
library(ggpubr)


#### GBIF Occurrence mapping

world.inp <- map_data("world")

#SET THE SPECIES LIST --------------

myspecies <-
  c("Carcinus maenas",
    "Callinectes sapidus",
    "Chthamalus fragilis",
    "Euchirograpsus americanus",
    "Hexapanopeus angustifrons",
    "Ocypode quadrata",
    "Palaemon floridanus",
    "Perophora viridis",
    "Aiptasiogeton eruptaurantia",
    "Centropristis striata",
    "Pterois miles",
    "Minuca pugnax",
    "Styela clava"
)


myspecies <- data.frame("species" = myspecies)

#DATA EXTRACTION: ------------

pdf('MarineRangeShifters.pdf')

for (i in 1:nrow(myspecies)) {
  gbif_data = occ_data(
    scientificName = myspecies[i, "species"],
    hasCoordinate = TRUE,
    hasGeospatialIssue = FALSE,
    continent = 'north_america',
    limit = 5000 #number of occurrences to load
  )
  
  data <-
    data.frame(species = myspecies[i, "species"], gbif_data$data[, c("decimalLongitude", "decimalLatitude", "year")])
  
  map <-    ggplot()+ geom_map(
      data = world.inp,
      map = world.inp,
      aes(x = long, y = lat, map_id = region),
      fill = "grey80"
    ) +
    xlim(-97, -60) +
    ylim(20, 60) +
    coord_fixed() 
  
  
  early <-  data %>% 
    filter(year <= 1980) %>% 
    group_by(year) %>% 
    summarise(lat = mean(decimalLatitude), 
              lon = mean(decimalLongitude)) %>% 
    ungroup() 
  
  early_map <- map + 
    geom_point(data = early, aes(lon, lat))+
    labs(title = myspecies[i, "species"], subtitle= "Pre-1980")
  
  late <-  data %>% 
    filter(year >= 2000) %>% 
    group_by(year) %>% 
    summarise(lat = mean(decimalLatitude), 
              lon = mean(decimalLongitude)) %>% 
    ungroup() 
  
  late_map <- map + 
    geom_point(data = late, aes(lon, lat))+
    labs(subtitle = "    

               Post 2000")
  

  #### calculate annual stats for each species
  data_stats <-  data %>%
    group_by(year) %>%
    summarise(
      meanlat = mean(decimalLatitude),
      minlat = min(decimalLatitude),
      maxlat = max(decimalLatitude)
    ) %>%
    ungroup()
  
  g1<-ggplot(data_stats) +
          geom_jitter(data= data, aes(x=year, y=decimalLatitude))+
          geom_smooth(aes(x = year, y = meanlat, color="Mean"), method="lm") +
          geom_smooth(aes(x = year, y = minlat, color="Min"), method="lm") +
          geom_smooth(aes(x = year, y = maxlat, color="Max"), method="lm") +
          labs(title="Latitude Shift",
                 y = "Latitude")


  figure <- ggarrange(early_map,late_map,g1,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
  print(figure)
}
dev.off()
