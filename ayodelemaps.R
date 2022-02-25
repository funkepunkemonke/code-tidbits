library(rgbif)
library(lubridate)
library(stringi)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpmisc)
library(maps)
library(raster)
library(data.table)

world.inp <- map_data("world")


#SET THE SPECIES LIST --------------
myspecies <- c('Abutilon theophrasti',
               'Acer tataricum',
               'Aegilops triuncialis',
               'Anthemis cotula',
               'Artemisia absinthium',
               'Avena fatua',
               'Berberis julianae',
               'Bunias orientalis',
               'Carduus pycnocephalus',
               'Dittrichia graveolens',
               'Erodium cicutarium',
               'Galium mollugo',
               'Hieracium aurantiacum',
               'Hieracium caespitosum',
               'Hieracium piloselloides',
               'Hieracium floribundum',
               'Hordeum murinum',
               'Hydrilla verticillata', 
               'Hypochaeris radicata',
               'Ipomoea batatas',
               'Ipomoea purpurea',
               'Lolium temulentum',
               'Nardus stricta',
               'Pennisetum ciliare',
               'Persicaria nepalensis',
               'Prunus padus',
               'Rorippa austriaca',
               'Rubus armeniacus',
               'Salsola tragus',
               'Tamarix parviflora',
               'Thlaspi arvense',
               'Ulex europaeus',
               'Veronica officinalis')

myspecies <- data.frame( "species" = myspecies)

#DATA EXTRACTION: ------------

for(i in 1:nrow(myspecies)){
  gbif_data = occ_data(scientificName = myspecies[i,"species"],
                 hasCoordinate = TRUE,
                 hasGeospatialIssue = FALSE,
                 continent='north_america',
                 limit = 1000)

    data <- data.frame(species = myspecies[i,"species"], gbif_data$data[ , c("decimalLongitude", "decimalLatitude")])
    plot <- data %>%
      ggplot(.) + 
      geom_map(data = world.inp, map = world.inp, aes(x = long, y = lat, map_id = region), fill = "grey80") +
      xlim(-97,-60) + 
      ylim(20, 60) + 
      coord_fixed() + aes(x=decimalLongitude, y=decimalLatitude) +geom_point() + 
      labs(title=myspecies[i,"species"])

      jpeg(file = paste("map_", myspecies[i,"species"], '.jpeg'))

      print(plot)

      graphics.off()
    }
    


